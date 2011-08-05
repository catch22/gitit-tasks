module Tasks (plugin) where

import Control.Monad.CatchIO (try)
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
import Text.Pandoc (defaultParserState, readMarkdown)


data Focus = Today | Next deriving (Eq, Show)
data Status = Open { focus :: Focus, due :: Maybe Day }
  | Completed { on :: Maybe Day }
  | Canceled { on :: Maybe Day }
  deriving Show
data Task = Task { status :: Status, title :: Block, content :: [Block] }


-- parse monad
data Parser a = Result a | ParseError String | NotATask

instance Monad Parser where
  -- composition
  Result x >>= f = f x
  ParseError str >>= _ = ParseError str
  NotATask >>= _ = NotATask

  -- lifts
  return = Result
  fail = ParseError

getResult :: Parser a -> Maybe a
getResult (Result x) = Just x
getResult _ = Nothing

--- try to parse list of blocks (e.g., the children blocks of a bullet list) as a task
parseTask :: [Block] -> Parser Task
parseTask (Plain is':bs) = parsePara is' Plain bs
parseTask (Para is':bs) = parsePara is' Para bs
parseTask _ = NotATask

parsePara :: [Inline] -> ([Inline] -> Block) -> [Block] -> Parser Task
parsePara (Str "[":Space:Str "]":Space:rest) block = return . Task (Open Today Nothing) (block rest)
parsePara (Str "[":Str ".":Str "]":Space:rest) block = return . Task (Open Next Nothing) (block rest)
parsePara (Str "[":Str "x":Str "]":Space:rest) block = return . Task (Completed Nothing) (block rest)
parsePara (Str "[":Str "/":Str "]":Space:rest) block = return . Task (Canceled Nothing) (block rest)
parsePara (Link [] (info, _):Space:rest) block = parseInfo info . Task (Open Today Nothing) (block rest)
parsePara (Link [Str "."] (info, _):Space:rest) block = parseInfo info . Task (Open Next Nothing) (block rest)
parsePara (Link [Str "x"] (info, _):Space:rest) block = parseInfo info . Task (Completed Nothing) (block rest)
parsePara (Link [Str "/"] (info, _):Space:rest) block = parseInfo info . Task (Canceled Nothing) (block rest)
parsePara _ _ = const NotATask

parseInfo :: String -> Task -> Parser Task
parseInfo str task = foldr (=<<) (return task) (map apply $ splitOn "," str)
  where
    apply :: String -> Task -> Parser Task
    apply str task = do
      status' <- apply' str (status task)
      return $ task { status = status' }

    apply' :: String -> Status -> Parser Status
    apply' "" x = return x
    apply' ('d':'u':'e':':':str) (Open focus _) = return $ Open focus (Just $ parseDate str)
    apply' ('d':'o':'n':'e':':':str) (Completed _) = return $ Completed (Just $ parseDate str)
    apply' ('d':'o':'n':'e':':':str) (Canceled _) = return $ Canceled (Just $ parseDate str)
    apply' str task = fail $ "Task modifier '" ++ str ++ "'" ++ " unknown or not applicable to status '" ++ show task ++ "'"

    parseDate :: String -> Day
    parseDate = readTime defaultTimeLocale "%Y-%m-%d"

-- find all top-level tasks in the given blocks
findToplevelTasks :: [Block] -> [Task]
findToplevelTasks = catMaybes . map getResult . concat . map go
  where
    go (BulletList items) = map parseTask items
    go _ = []


-- format task back into a list of blocks
formatTask :: Day -> Task -> [Block]
formatTask today (Task (Open Today due) title content) = formatTitle (Just today) id id due title : content
formatTask today (Task (Open Next due) title content) = formatTitle (Just today) emph id due title : content where emph is = [Emph is]
formatTask _ (Task (Completed on) title content) = formatTitle Nothing prefix id on title : content
  where
    prefix is = Emph [Str "☒ Completed:"] : Space : is
formatTask _ (Task (Canceled on) title content) = formatTitle Nothing prefix strikeout on title : content
  where
    prefix is = Emph [Str "☒ Canceled:"] : Space : is
    strikeout is = [Strikeout is]   --- does not do anything due to Pandoc bug

formatTitle :: Maybe Day -> ([Inline] -> [Inline]) -> ([Inline] -> [Inline]) -> Maybe Day -> Block -> Block
formatTitle compareToday prefix wrap day (Plain is) = Plain $ prefix $ formatDay compareToday day ++ wrap is
formatTitle compareToday prefix wrap day (Para is) = Para $ prefix $ formatDay compareToday day ++ wrap is

formatDay :: Maybe Day -> Maybe Day -> [Inline]
formatDay compareToday (Just day) = (decorate compareToday) [Str (formatTime defaultTimeLocale "%b %e, %Y" day), Str ":", Space]
  where
    decorate (Just today) is | not (today < day) = [Strong is]
    decorate _ is = is
formatDay _ Nothing = []


-- current day in current timezone
getCurrentLocalDay :: IO Day
getCurrentLocalDay = do
  timezone <- getCurrentTimeZone
  time <- getCurrentTime
  return $ localDay $ utcToLocalTime timezone time


-- gitit plugin entry point
plugin :: Plugin
plugin = mkPageTransformM transformTasks

-- transform page block by block..
transformTasks :: Block -> PluginM Block

-- ..formatting tasks in bullet lists
transformTasks (BulletList items) = do
    -- do not cache (time-dependent)
    doNotCache

    -- determine current day
    today <- liftIO getCurrentLocalDay

    -- list all open tasks
    return $ BulletList (catMaybes (map (transformItem today) items))
  where
    transformItem :: Day -> [Block] -> Maybe [Block]
    transformItem today bs = case parseTask bs of
      Result task -> if showTask task then Just (formatTask today task) else Nothing
      ParseError str -> Just [Plain [RawInline "html" "<font color='red'>", Str str, RawInline "html" "</font>"]]
      NotATask -> Just bs

    showTask :: Task -> Bool
    --showTask _ = True
    showTask (Task (Open _ _) _ _) = True
    showTask _ = False

-- ..aggregating tasks from other wiki pages
transformTasks (Para [Link [Str "!", Str "tasks"] (url, _)]) = do
  -- do not cache (time-dependent highlighting, dynamic aggregation)
  doNotCache

  -- get current day
  today <- liftIO getCurrentLocalDay

  -- load page from filestore
  cfg <- askConfig
  let filestore = filestoreFromConfig cfg
  let Just pageName = decString True url
  page <- try $ liftIO (retrieve filestore (pageName ++ ".page") Nothing)
  case page :: Either FileStoreError String of
    Left e ->
      -- does not exist? output ordinary wiki link
      let
        label = Str ("[!tasks](" ++ pageName ++ ")")
        alt = "'" ++ pageName ++ "' doesn't exist. Click here to create it."
      in
        return $ Para [Link [label] (pageName, alt)]
    Right markdown ->
      -- markdown found? collect all tasks into a single bullet list
      let
        Pandoc _ content = readMarkdown defaultParserState markdown
        tasks = filter showTask (findToplevelTasks content)
        showTask (Task (Open Today _) _ _) = True
        showTask (Task (Open Next (Just due)) _ _) | not (today < due) = True
        showTask _ = False
      in
        return $ BulletList (map (formatTask today) tasks)

transformTasks other = return other
