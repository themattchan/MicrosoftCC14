import Data.List.Split


splitDrome :: String -> (String, String)
splitDrome line = let (x:y:_) = splitOn "|" line in
                  (x, y)

dromeTest :: (String, String) -> String
dromeTest (str, n) = let chunked = chunksOf (read n) str in
                     if chunked == reverse chunked
                     then "|1"
                     else "|0"

removeCRLF :: String -> String
removeCRLF line = let (x,y) = splitDrome line in
                    x ++ "|" ++ y

main :: IO ()
main = do
  f <- readFile "ActualInput.txt"
  let dromes = lines f
      tested = map (dromeTest . splitDrome) dromes
      result = zipWith (++) (map removeCRLF dromes) tested
  writeFile "output" $ unlines result
