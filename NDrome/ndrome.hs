import Data.List.Split
splitDrome :: String -> (String, Int)
splitDrome line = let sp = splitOn "|" line in
                  (head sp, read $ head $ tail sp)

dromeTest (str, n) = let chunked = chunksOf n str in
                     case (chunked == reverse chunked) of
                       True  -> "|1"
                       False -> "|0"

removeCRLF line = case splitDrome line of
                    (x,y) -> x++"|"++(show y)

main = do
  f <- readFile "ActualInput.txt"
  let dromes = lines f
      tested = map (dromeTest . splitDrome) dromes
      result = zipWith (++) (map removeCRLF dromes) tested
  writeFile "output" $ unlines result
