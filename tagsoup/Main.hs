{-# LANGUAGE OverloadedStrings #-}
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Time (LocalTime, parseTimeM, defaultTimeLocale)
import           Data.Tree.NTree.TypeDefs
import           Text.HandsomeSoup
import           Text.XML.HXT.Core
import Control.Applicative

data MatchInfo =
  MatchInfo { round      :: Int,
              venue      :: String,
              date       :: LocalTime,
              attendance :: Int
            } deriving Show

type Player = String
type Team = String

data ScoreType = Goal | Behind | RushedBehind
  deriving Show

data TeamType = Home | Away
  deriving Show

data Teams = Teams
  { home :: Team
  , away :: Team }

data ScoringEvent = ScoringEvent
  { teamType :: TeamType
  , time :: Int
  , scoreType :: ScoreType
  , scorer :: Maybe Player
  }
  deriving Show

readScoringEvent :: [String] -> ScoringEvent
readScoringEvent xs = case head xs of
  "\160" -> uncurry3 (ScoringEvent Away) $ fromLine $ reverse xs
  _ -> uncurry3 (ScoringEvent Home) $ fromLine xs
  where
   fromLine :: [String] -> (Int, ScoreType, Maybe Player)
   fromLine (description:t':_) =
      let t = readTime t'
      in
        if description == "Rushed Behind"
          then (t, RushedBehind, Nothing)
          else case reverse . words $ description of
                "behind":name -> (t, Behind, Just $ unwords name)
                "goal":name -> (t, Goal, Just $ unwords name)
                _ -> error $ "Unexpected goal type in: " ++ description
   fromLine xs = error $ "couldnt parse " ++ (unwords xs)


trim :: String -> String
trim = unwords . words

readMatchInfo :: [String] -> MatchInfo
readMatchInfo [round, venue, date, attendance]
  = MatchInfo (read round) venue (readDate date) (read attendance)

readDate :: String -> LocalTime
readDate t = fromJust $ parseTimeM True defaultTimeLocale fmt t'
  where
    t' = trim $ takeWhile (/='(') t
    trim = unwords . words
    fmt = "%a, %e-%b-%Y %l:%M %p"

matchInfoArr :: IOSLA (XIOState ()) (NTree XNode) MatchInfo
matchInfoArr = (css "table:first-child" >>>
                css "tr:first-child" >>>
                css "td:nth-child(2)" >>>
               removeAllWhiteSpace >>>
                (deep getText))
               >. (readMatchInfo . fmap trim . getElems [1,3,5,7])
  where getElems is xs = map (xs !!) is

-- scoringLineArr :: ArrowXml a => a (NTree XNode) [ScoringEvent]
scoringLineArr =  css "td" >>>
                  listA (deep getText >>. unwords)

scoringTableArr =
  (css "table:nth-child(8)")
  >>> css "tr"
  >>> listA scoringLineArr

scoringTableTeamsArr :: IOSLA (XIOState ()) (NTree XNode) (String, String)
scoringTableTeamsArr =
  (css "table:nth-child(8)" >>>
  css "tr:nth-child(2)"
  >>> removeAllWhiteSpace
  >>> css "th"
  //> getText)
  >. (liftA2 (,) head last)

readTime :: String -> Int
readTime s =
  let
    rd = read :: String -> Int
    [min,sec] = rd . init <$> words s
  in
    60*min + sec
getScore = do
  html <- readFile "test.html"
  let doc = readString [withParseHTML yes, withWarnings no] html
  -- r <- runX $ doc >>> matchInfoArr
  r <- runX $ doc >>> scoringTableTeamsArr
  r <- runX $ doc >>>  scoringTableArr
  -- r <- runX $ doc >>> scoringTableTeamsArr
  let
    r' = tail . tail . init $ r  -- last and first 2 rows
    pred = and . map ("quarter" `isInfixOf`)
    events = (fmap readScoringEvent) <$> wordsBy pred r'
    quarters = filter pred r'
  return $ zip quarters events
  -- return r
