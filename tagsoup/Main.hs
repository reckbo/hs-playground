import           Data.Maybe
import           Data.Time
import           Text.HandsomeSoup
import           Text.XML.HXT.Core
import           Data.Tree.NTree.TypeDefs

trim :: String -> String
trim = unwords . words

data MatchInfo =
  MatchInfo { round      :: Int,
              venue      :: String,
              date       :: LocalTime,
              attendance :: Int
            } deriving Show

readMatchInfo :: [String] -> MatchInfo
readMatchInfo [round, venue, date, attendance]
  = MatchInfo (read round) venue (readDate date) (read attendance)

readDate :: String -> LocalTime
readDate t = fromJust $ parseTimeM True defaultTimeLocale fmt t'
  where
    t' = takeWhile (/='(') t
    fmt = "%a, %e-%b-%Y %l:%M %p"

matchInfoArr :: IOSLA (XIOState ()) (NTree XNode) MatchInfo
matchInfoArr = (css "table:first-child" >>>
                css "tr:first-child" >>>
                css "td:nth-child(2)" >>>
                (deep getText)) >.
               (readMatchInfo . fmap trim . getElems [1,3,6,8])
  where getElems is xs = map (xs !!) is

scoringLineArr :: ArrowXml a => a (NTree XNode) String
scoringLineArr =  css "td" >>>
                  listA (deep getText >>. unwords)

scoringTableArr = (css "table:nth-child(8)") >>>
                  css "tr" >>>
                  listA scoringLineArr

main = do
  html <- readFile "test2.html"
  let doc = readString [withParseHTML yes, withWarnings no] html
  -- r <- runX $ doc >>> matchInfoArr
  r <- runX $ doc >>> scoringTableArr
  print r
