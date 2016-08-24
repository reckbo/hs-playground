import Text.XML.HXT.Core
import Text.HandsomeSoup

main = do
  html <- readFile "test.html"
  let doc = readString [withParseHTML yes, withWarnings no] html
  -- tbl <- runX . xshow $ doc >>> css "table:first-child" >>> css "a" >>> getAttrValue "href"
  tbl <- runX $ doc >>> css "table:first-child" >>> css "tr:first-child" >>> css "a" >>> getAttrValue "href"
  -- links <- runX $ doc //> hasName "a" >>> getAttrValue "href"
  -- mapM_ putStrLn links
  -- putStrLn tbl
  mapM_ putStrLn tbl
