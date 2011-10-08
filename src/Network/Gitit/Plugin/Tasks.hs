{-# LANGUAGE PatternGuards #-}
module Network.Gitit.Plugin.Tasks (plugin) where

import Control.Monad
import Control.Monad.CatchIO (try)
import Data.Char (toUpper)
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
import System.FilePath
import System.Locale
import Text.Pandoc (defaultParserState, ParserState(..), readMarkdown)


data Focus = Today | Someday deriving (Eq, Show)
data Status =
    Open { focus :: Focus, due :: Maybe Day }
  | Completed { on :: Maybe Day }
  | Canceled { on :: Maybe Day }
    deriving (Eq, Show)
data Task = Task { status :: Status, delegates :: [String], tags :: [String], title :: Block, content :: [Block] } deriving (Eq, Show)

isOpen :: Status -> Bool
isOpen (Open _ _) = True
isOpen _ = False

isToday :: Status -> Bool
isToday (Open Today _) = True
isToday _ = False

dueDate :: Status -> Maybe Day
dueDate status = case status of
  Open _ due -> due
  Completed due -> due
  Canceled due -> due

isDue :: Day -> Status -> Bool
isDue today status | Just due <- dueDate status = isOpen status && not (today < due)
                   | otherwise = False

isSoonDue :: Day -> Status -> Bool
isSoonDue today status = isDue (addDays 2 today) status

isDelegatedTo :: Task -> String -> Bool
isDelegatedTo task username = username `elem` delegates task

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
    convert NotAvailable (b:bs) = Left $ Task (Open Today Nothing) [] [] b bs

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
      let task' = task { delegates = delegates task ++ [name] }
      parseMeta task' rest
    parseMeta task (Str "#":Str name:Space:rest) = do
      let task' = task { tags = tags task ++ [name] }
      parseMeta task' rest
    parseMeta task rest = return $ task { title = makeBlock rest }

    updateStatusWithDate :: Day -> Status -> Parser Status
    updateStatusWithDate due (Open focus _) = return $ Open focus (Just due)
    updateStatusWithDate done (Completed _) = return $ Completed (Just done)
    updateStatusWithDate done (Canceled _) = return $ Canceled (Just done)

    taskForStatus :: Inline -> Block -> [Block] -> Parser Task
    taskForStatus Space title = return . Task (Open Today Nothing) [] [] title
    taskForStatus (Str "?") title = return . Task (Open Someday Nothing) [] [] title
    taskForStatus (Str "x") title = return . Task (Completed Nothing) [] [] title
    taskForStatus (Str "/") title = return . Task (Canceled Nothing) [] [] title
    taskForStatus _ _ = const NotAvailable
parsePara _ _ _ = NotAvailable

-- find all top-level tasks in the given blocks
findToplevelTasks :: [Block] -> [Task]
findToplevelTasks = lefts . concat . catMaybes . map parseBlock
  where
    parseBlock (BulletList items) = parseTaskList items
    parseBlock _ = Nothing


-- wrapping utilities
wrapWithSpan :: String -> [Inline] -> [Inline]
wrapWithSpan class_ inlines = [RawInline "html" ("<span class=\"" ++ class_ ++ "\">")] ++ inlines ++ [RawInline "html" "</span>"]

wrapWithDiv :: String -> [Block] -> [Block]
wrapWithDiv class_ blocks = [RawBlock "html" ("<div class=\"" ++ class_ ++ "\">")] ++ blocks ++ [RawBlock "html" "</div>"]

updateWrapped :: Block -> ([Inline] -> [Inline]) -> Block
updateWrapped (Plain inlines) f = Plain (f inlines)
updateWrapped (Para inlines) f = Para (f inlines)

-- format task as a list of blocks
formatTask :: Day -> Task -> [Block]
formatTask today (Task status delegates tags title content) = updateWrapped title update : content
  where
    update inlines = statusBox : wrapWithSpan "tasks-header" (dueDatePrefix ++ delegatePrefix ++ tagPrefix ++ inlines)

    statusBox = RawInline "html" $ "<input type=\"checkbox\" class=\"tasks-status" ++ (case status of
        Open Someday _ -> " tasks-someday"
        otherwise -> "") ++
      "\"" ++ (case status of
        Open _ _ -> ""
        Completed _ -> " checked=\"checked\""
        Canceled _ -> " disabled=\"disabled\"") ++
      "\" />"

    dueDatePrefix | Just due <- dueDate status = wrapDueDate [Str (formatTime defaultTimeLocale "%b %e, %Y" due), Str ":", Space]
                  | otherwise = []

    wrapDueDate | isDue today status = wrapWithSpan "tasks-due"
                | isSoonDue today status = wrapWithSpan "tasks-soondue"
                | otherwise = id

    delegatePrefix = concat [[Link (wrapWithSpan "tasks-delegate-at" [Str "@"] ++ wrapWithSpan "tasks-delegate-name" [Str name]) ("/Users/" ++ name, "")] ++ [Space] | name <- delegates]

    tagPrefix = concat [[Link (wrapWithSpan "tasks-tag-hash" [Str "#"] ++ wrapWithSpan "tasks-tag-name" [Str name]) ("/Tags/" ++ name, "")] ++ [Space] | name <- tags]


-- current day in current timezone
getCurrentLocalDay :: IO Day
getCurrentLocalDay = do
  timezone <- getCurrentTimeZone
  time <- getCurrentTime
  return $ localDay $ utcToLocalTime timezone time

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
transformBlock (Plain [Link [Str "!", Str title] (url, _)]) | Just showTaskM <- lookup title aggregators = showTaskM >>= aggregateTasks url
transformBlock (Para [Link [Str "!", Str title] (url, _)]) | Just showTaskM <- lookup title aggregators = showTaskM >>= aggregateTasks url
transformBlock other = return [other]

aggregators :: [(String, PluginM (Task -> Bool))]
aggregators = [("tasks", ((isUndelegatedM `orP` isDelegatedToMeM) `andP` isTodayM) `orP` isSoonDueM),
    ("duetasks", isSoonDueM),
    ("tasksdelegatedtome", isDelegatedToMeM `andP` (isTodayM `orP` isSoonDueM))]
  where
    isUndelegatedM :: PluginM (Task -> Bool)
    isUndelegatedM = return ((== []) . delegates)

    isTodayM :: PluginM (Task -> Bool)
    isTodayM = return (isToday . status)

    isSoonDueM :: PluginM (Task -> Bool)
    isSoonDueM = do
      doNotCache
      today <- liftIO getCurrentLocalDay
      return (isSoonDue today . status)

    isDelegatedToMeM :: PluginM (Task -> Bool)
    isDelegatedToMeM = do
      doNotCache
      user <- askUser
      let username = liftM (capitalize . uUsername) user where capitalize (c:cs) = toUpper c:cs
      return $ \task -> isOpen (status task) && maybe False (task `isDelegatedTo`) username

    orP, andP :: PluginM (Task -> Bool) -> PluginM (Task -> Bool) -> PluginM (Task -> Bool)
    left `orP` right = do
      f <- left
      g <- right
      return $ \task -> f task || g task
    left `andP` right = do
      f <- left
      g <- right
      return $ \task -> f task && g task


-- format task list
formatTaskList :: [[Block]] -> PluginM [Block]
formatTaskList items = do
  doNotCache
  today <- liftIO getCurrentLocalDay
  showTask <- getTaskFilter
  return $ case parseTaskList items of
    Just results -> wrapWithDiv "tasks" [BulletList $ catMaybes $ map formatResult results]
      where
        formatResult :: Either Task String -> Maybe [Block]
        formatResult (Left task) = if showTask (status task) then Just (formatTask today task) else Nothing
        formatResult (Right error) = Just [Plain $ wrapWithSpan "tasks-error" [Str error]]
    Nothing -> [BulletList items]

getTaskFilter :: PluginM (Status -> Bool)
getTaskFilter = do
  meta <- askMeta
  return $ case lookup "tasks" meta of
    Just "all" -> const True
    _ -> isOpen

-- aggregate tasks from other wiki pages
aggregateTasks :: String -> (Task -> Bool) -> PluginM [Block]
aggregateTasks pageNameURL showTask = do
  doNotCache
  today <- liftIO getCurrentLocalDay

  cfg <- askConfig
  let filestore = filestoreFromConfig cfg
  let Just pageName = decString True pageNameURL

  ctx <- getContext
  let cwd = '/' : takeDirectory (ctxFile ctx)
  let ('/':absPageName) = combine cwd pageName

  page <- try $ liftIO (retrieve filestore (absPageName ++ ".page") Nothing)

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
        tasks = filter showTask (findToplevelTasks content)
      in
        if tasks == [] then [] else wrapWithDiv "tasks" [BulletList $ map (formatTask today) tasks]
