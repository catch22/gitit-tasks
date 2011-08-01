module Tasks (plugin) where

import Data.List.Split
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import Network.Gitit.Interface
import System.Locale


data Focus = Today | Next deriving Eq
data Status = Open { focus :: Focus, due :: Maybe Day }
  | Completed { on :: Maybe Day }
  | Canceled { on :: Maybe Day }
data Task = Task { status :: Status, title :: Block, content :: [Block] }

parseTask :: [Block] -> Maybe Task
parseTask (Plain is':bs) = parsePara is' Plain bs
parseTask (Para is':bs) = parsePara is' Para bs
parseTask _ = Nothing

parsePara :: [Inline] -> ([Inline] -> Block) -> [Block] -> Maybe Task
parsePara (Str "[":Space:Str "]":Space:rest) block = Just . Task (Open Today Nothing) (block rest)
parsePara (Str "[":Str "x":Str "]":Space:rest) block = Just . Task (Completed Nothing) (block rest)
parsePara (Str "[":Str "/":Str "]":Space:rest) block = Just . Task (Canceled Nothing) (block rest)
parsePara (Link [] (info, _):Space:rest) block = Just . parseInfo info . Task (Open Today Nothing) (block rest)
parsePara (Link [Str "x"] (info, _):Space:rest) block = Just . parseInfo info . Task (Completed Nothing) (block rest)
parsePara (Link [Str "/"] (info, _):Space:rest) block = Just . parseInfo info . Task (Canceled Nothing) (block rest)
parsePara _ _ = \_ -> Nothing

parseInfo :: String -> Task -> Task
parseInfo str = foldr (.) id (map apply $ splitOn "," str)
  where
    apply :: String -> (Task -> Task)
    apply str task = task { status = apply' str (status task) }

    apply' :: String -> (Status -> Status)
    apply' "" x = x
    apply' "next" (Open _ due) = Open Next due
    apply' ('d':'u':'e':':':str) (Open focus _) = Open focus (Just $ parseDate str)
    apply' ('d':'o':'n':'e':':':str) (Completed _) = Completed (Just $ parseDate str)
    apply' ('d':'o':'n':'e':':':str) (Canceled _) = Canceled (Just $ parseDate str)
    apply' str _ = error $ "Unknown task modifier '" ++ str ++ "'"

    parseDate :: String -> Day
    parseDate = readTime defaultTimeLocale "%Y-%m-%d"

formatTask :: Task -> [Block]
formatTask (Task (Open Today due) title content) = formatTitle id id due title : content
formatTask (Task (Open Next due) title content) = formatTitle emph id due title : content where emph is = [Emph is]
formatTask (Task (Completed on) title content) = formatTitle prefix id on title : content
  where
    prefix is = Emph [Str "☒ Completed:"] : Space : is
formatTask (Task (Canceled on) title content) = formatTitle prefix strikeout on title : content
  where
    prefix is = Emph [Str "☒ Canceled:"] : Space : is
    strikeout is = [Strikeout is]   --- does not do anything due to Pandoc bug

formatTitle :: ([Inline] -> [Inline]) -> ([Inline] -> [Inline]) -> Maybe Day -> Block -> Block
formatTitle prefix wrap day (Plain is) = Plain $ prefix $ formatDay day ++ wrap is
formatTitle prefix wrap day (Para is) = Para $ prefix $ formatDay day ++ wrap is

formatDay :: Maybe Day -> [Inline]
formatDay (Just day) = [Str (formatTime defaultTimeLocale "%b %e, %Y" day), Str ":", Space]
formatDay Nothing = []


plugin :: Plugin
plugin = mkPageTransformM transformTasks

transformTasks :: Block -> PluginM Block
transformTasks (BulletList items) = return $ BulletList (catMaybes (map transformItem items))
  where
    transformItem :: [Block] -> Maybe [Block]
    transformItem bs = case parseTask bs of
      Just task -> if showTask task then Just (formatTask task) else Nothing
      Nothing -> Just bs

    showTask :: Task -> Bool
    --showTask _ = True
    showTask (Task (Open _ _) _ _) = True
    showTask _ = False
transformTasks other = return other
