module Tasks (plugin) where

import Control.Monad.CatchIO (try)
import Data.Either
import Data.FileStore (FileStoreError, retrieve)
import Data.List.Split
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Network.Gitit.Interface
import Network.Gitit.Framework (filestoreFromConfig)
import Network.Gitit.ContentTransformer (inlinesToString)
import Network.URL (decString)
import System.Locale
import Text.Pandoc (defaultParserState, ParserState(..), readMarkdown)


data Focus = Today | Someday deriving (Eq, Show)
data Status =
    Open { focus :: Focus, due :: Maybe Day }
  | Completed { on :: Maybe Day }
  | Canceled { on :: Maybe Day }
    deriving (Eq, Show)
data Task = Task { status :: Status, delegate :: Maybe String, title :: Block, content :: [Block] } deriving (Eq, Show)


-- parse monad
data Parser a = Result a | ParseError String | NotAvailable deriving (Eq)

instance Monad Parser where
  -- composition
  Result x >>= f = f x
  ParseError str >>= _ = ParseError str
  NotAvailable >>= _ = NotAvailable

  -- lifts
  return = Result
  fail = ParseError

-- parse bullet list (items) as a task list
parseTaskList :: [[Block]] -> Maybe [Either Task String]
parseTaskList items = case map parseTask items of
    results | all (== NotAvailable) results -> Nothing
            | otherwise -> Just $ map (uncurry convert) (zip results items)
  where
    convert :: Parser Task -> [Block] -> Either Task String
    convert (Result task) _ = Left task
    convert (ParseError str) _ = Right str
    convert NotAvailable (b:bs) = Left $ Task (Open Today Nothing) Nothing b bs

-- parse list of blocks (e.g., a bullet list item) as a task
parseTask :: [Block] -> Parser Task
parseTask (Plain is':bs) = parsePara is' Plain bs
parseTask (Para is':bs) = parsePara is' Para bs
parseTask _ = NotAvailable

parsePara :: [Inline] -> ([Inline] -> Block) -> [Block] -> Parser Task
parsePara (Str "[":statusInline:Str "]":Space:rest) makeBlock content = do
    task <- taskForStatus statusInline (Plain [Str "(parse in progress)"]) content
    parseMeta task rest
  where
    parseMeta :: Task -> [Inline] -> Parser Task
    parseMeta task all@(Str year:EnDash:Str month:EnDash:Str day:Space:rest) =
      case parseTime defaultTimeLocale "%Y-%m-%d" (year ++ "-" ++ month ++ "-" ++ day) of
        Just date -> do
          status' <- updateStatusWithDate date (status task)
          let task' = task { status = status' }
          parseMeta task' rest
        Nothing -> return $ task { title = makeBlock all }
    parseMeta task (Str ('@':name):Space:rest) = do
      let task' = task { delegate = Just name }
      parseMeta task' rest
    parseMeta task rest = return $ task { title = makeBlock rest }

    updateStatusWithDate :: Day -> Status -> Parser Status
    updateStatusWithDate due (Open focus _) = return $ Open focus (Just due)
    updateStatusWithDate done (Completed _) = return $ Completed (Just done)
    updateStatusWithDate done (Canceled _) = return $ Canceled (Just done)

    taskForStatus :: Inline -> Block -> [Block] -> Parser Task
    taskForStatus Space title = return . Task (Open Today Nothing) Nothing title
    taskForStatus (Str "?") title = return . Task (Open Someday Nothing) Nothing title
    taskForStatus (Str "x") title = return . Task (Completed Nothing) Nothing title
    taskForStatus (Str "/") title = return . Task (Canceled Nothing) Nothing title
    taskForStatus _ _ = const NotAvailable
parsePara _ _ _ = NotAvailable

-- find all top-level tasks in the given blocks
findToplevelTasks :: [Block] -> [Task]
findToplevelTasks = lefts . concat . catMaybes . map parseBlock
  where
    parseBlock (BulletList items) = parseTaskList items
    parseBlock _ = Nothing


-- format task back into a list of blocks
formatTask :: Day -> Task -> [Block]
formatTask today (Task (Open Today due) delegate title content) = formatTitle (Just today) (prefixDelegate delegate) id due title : content
formatTask today (Task (Open Someday due) delegate title content) = formatTitle (Just today) (prefixDelegate delegate . emph) id due title : content
  where emph is = [Emph is]
formatTask _ (Task (Completed done) delegate title content) = formatTitle Nothing (prefixDone "☒ Completed:" . prefixDelegate delegate) id done title : content
formatTask _ (Task (Canceled done) delegate title content) = formatTitle Nothing (prefixDone "☒ Canceled:" . prefixDelegate delegate) strikeout done title : content
  where strikeout is = [Strikeout is]   --- does not do anything due to Pandoc bug

prefixDelegate :: Maybe String -> [Inline] -> [Inline]
prefixDelegate (Just name) is = Link [Str ('@':name)] ('@':name,"") : Space : is
prefixDelegate Nothing is = is

prefixDone :: String -> [Inline] -> [Inline]
prefixDone str is = Emph [Str str] : Space : is

formatTitle :: Maybe Day -> ([Inline] -> [Inline]) -> ([Inline] -> [Inline]) -> Maybe Day -> Block -> Block
formatTitle maybeToday prefix wrap day (Plain is) = Plain $ prefix $ formatDay maybeToday day ++ wrap is
formatTitle maybeToday prefix wrap day (Para is) = Para $ prefix $ formatDay maybeToday day ++ wrap is

formatDay :: Maybe Day -> Maybe Day -> [Inline]
formatDay maybeToday (Just day) = (decorate maybeToday) [Str (formatTime defaultTimeLocale "%b %e, %Y" day), Str ":", Space]
  where
    decorate (Just today) is | not (today < day) = [RawInline "html" "<font color='red'>", Strong is, RawInline "html" "</font>"]
    decorate (Just today) is | not (today < warnDay day) = [Strong is]
    decorate _ is = is
formatDay _ Nothing = []

warnDay :: Day ->Day
warnDay = addDays (-2)

-- current day in current timezone
getCurrentLocalDay :: IO Day
getCurrentLocalDay = do
  timezone <- getCurrentTimeZone
  time <- getCurrentTime
  return $ localDay $ utcToLocalTime timezone time

-- wrap blocks in task list <div>
wrapInDiv :: Block -> [Block]
wrapInDiv block = [RawBlock "html" "<div class=\"tasks\">", block, RawBlock "html" "</div>"]


-- gitit plugin entry point
plugin :: Plugin
plugin = mkPageTransformM transformBlocks
  where
    transformBlocks :: [Block] -> PluginM [Block]
    transformBlocks (b:bs) = do
      bs' <- transformBlock b
      bs'' <- transformBlocks bs
      return $ bs' ++ bs''
    transformBlocks [] = return []

-- transform page block by block..
transformBlock :: Block -> PluginM [Block]
transformBlock (BulletList items) = formatTaskList items
transformBlock (Plain [Link [Str "!", Str "tasks"] (url, _)]) = aggregateTasks url
transformBlock (Para [Link [Str "!", Str "tasks"] (url, _)]) = aggregateTasks url
transformBlock other = return [other]

-- format task list
formatTaskList :: [[Block]] -> PluginM [Block]
formatTaskList items = do
  doNotCache
  today <- liftIO getCurrentLocalDay
  showTask <- getTaskFilter
  return $ case parseTaskList items of
    Just results -> wrapInDiv $ BulletList $ catMaybes $ map formatResult results
      where
        formatResult :: Either Task String -> Maybe [Block]
        formatResult (Left task) = if showTask (status task) then Just (formatTask today task) else Nothing
        formatResult (Right error) = Just [Plain [RawInline "html" "<font color='red'>", Strong [Str error], RawInline "html" "</font>"]]
    Nothing -> [BulletList items]

getTaskFilter :: PluginM (Status -> Bool)
getTaskFilter = do
  meta <- askMeta
  return $ case lookup "tasks" meta of
    Just "all" -> const True
    _ -> isOpen
  where
    isOpen :: Status -> Bool
    isOpen (Open _ _) = True
    isOpen _ = False

-- aggregate tasks from other wiki pages
aggregateTasks :: String -> PluginM [Block]
aggregateTasks pageNameURL = do
  doNotCache
  today <- liftIO getCurrentLocalDay

  cfg <- askConfig
  let filestore = filestoreFromConfig cfg
  let Just pageName = decString True pageNameURL
  page <- try $ liftIO (retrieve filestore (pageName ++ ".page") Nothing)

  return $ case page :: Either FileStoreError String of
    Left e ->
      -- does not exist? output ordinary wiki link
      let
        label = Str ("[!tasks](" ++ pageName ++ ")")
        alt = "'" ++ pageName ++ "' doesn't exist. Click here to create it."
      in
        [Para [Link [label] (pageName, alt)]]
    Right markdown ->
      -- markdown found? collect all tasks into a single bullet list
      let
        Pandoc _ content = readMarkdown (defaultParserState { stateSmart = True }) markdown
        tasks = filter (showTask . status) (findToplevelTasks content)

        showTask (Open Today _) = True   -- undelegated and today
        showTask (Open _ (Just due)) | not (today < due) = True   -- due
        showTask _ = False
      in
        wrapInDiv $ BulletList (map (formatTask today) tasks)
