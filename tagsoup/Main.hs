import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Time
import Data.Maybe

data MatchInfo =
  MatchInfo { round :: Int,
              venue :: String,
              date :: LocalTime,
              attendance :: Int
            }
  deriving Show

readMatchInfo :: [String] -> MatchInfo
readMatchInfo [round, venue, date, attendance]
  = MatchInfo (read round) venue (readDate date) (read attendance)

readDate :: String -> LocalTime
readDate t = fromJust $ parseTimeM True defaultTimeLocale fmt t'
  where
    t' = takeWhile (/='(') t
    fmt = "%a, %e-%b-%Y %l:%M %p"

trim :: String -> String
trim = unwords . words

matchInfoArr = (css "table:first-child" >>> css "tr:first-child" >>> css "td:nth-child(2)" >>> (deep getText)) >. (readMatchInfo . fmap trim . getElems [1,3,6,8])
  where getElems is xs = map (xs !!) is

main = do
  html <- readFile "test.html"
  let doc = readString [withParseHTML yes, withWarnings no] html
  r <- runX $ doc >>> matchInfoArr
  print r
