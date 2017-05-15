-- Following the programming exercise/tutorial from Chalmers University:
-- http://www.cse.chalmers.se/edu/year/2010/course/TDA451_Functional_Programming/labs/3/

{-# LANGUAGE TupleSections #-}
module Sudoku where
import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Functor
import Control.Monad

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
              deriving (Show, Eq)

-- `allBlankSudoku` is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku . replicate 9 . replicate 9 $ Nothing

example :: Sudoku
example =
    Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]

-- `isSudoku s` checks if `s` is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku s)      = validRows && validCols && validNums
  where validRows        = length s == 9
        validCols        = and $ map ((== 9) . length) s
        validNums        = and $ concatMap (map isValid) s
        isValid Nothing  = True
        isValid (Just n) = n >= 1 && n <= 9

-- `isSolved s` checks if `s` is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved = (Nothing `notElem`) . concat . rows

-- `printSudoku s` prints a representation of the sudoku `s` on the screen
printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . prettySudoku

prettySudoku :: Sudoku -> String
prettySudoku =  unlines . map (unwords . map printCell) . rows
  where printCell = fromMaybe "." . fmap show

-- `readSudoku file` reads from the `file`, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = readFile >=> return . Sudoku . map parseRow . lines
  where parseRow = map parseChar
        parseChar x | x == '.'            = Nothing
                    | x `elem` ['1'..'9'] = Just (digitToInt x)
                    | otherwise           = error "not a sudoku puzzle"

-------------------------------------------------------------------------

-- `cell` generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = arbitrary `suchThatMaybe` (\x -> x >= 0 && x <= 9)

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock xs = let xs' = catMaybes xs in
  length xs' == (length . nub) xs'

blocks :: Sudoku -> [Block]
blocks (Sudoku s) = s ++ transpose s ++ nineSquares s

blocksSet :: Sudoku -> ([Block], [Block], [Block])
blocksSet (Sudoku s) = (s, transpose s, nineSquares s)

nineSquares :: [Block] -> [Block]
nineSquares = concatMap (tbtBlock . colGroups) . rowGroups
  where rowGroups = chunksOf 3
        colGroups = map (chunksOf 3)
        tbtBlock  = concat . transpose

isOkay :: Sudoku -> Bool
isOkay = and . map isOkayBlock . blocks

type Pos = (Int,Int)

indexedPuzzle :: Sudoku -> [(Int, [(Int, Maybe Int)])]
indexedPuzzle = zip [0..8] . map (zip [0..8]) . rows

blanks :: Sudoku -> [Pos]
blanks = toPosList . getBlanks . indexedPuzzle
  where getBlanksRow = map fst . filter (isNothing . snd)
        getBlanks    = map (fmap getBlanksRow)
        toPosList    = concatMap (\(r, xs) -> map (r,) xs)

(!!=) :: [a] -> (Int, a) -> [a]
(!!=) []      _                = []
(!!=) (x:xs) (i,v) | i == 0    = v : xs
                   | otherwise = x : (xs !!= (i-1, v))

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku s) (r,c) i = Sudoku newGrid
  where newGrid = take r s ++ [(s!!r) !!= (c,i)] ++ drop (r+1) s

candidates :: Sudoku -> Pos -> [Int]
candidates (Sudoku s) (r,c) =
  [1..9] \\ (map (fromMaybe 0) (rowCands ++ colCands ++ boxCands))
  where (sr,sc,sb) = blocksSet (Sudoku s)
        rowCands   = sr !! r
        colCands   = sc !! c
        boxCands   = sb !! ((r `div` 3) * 3 + (c `div` 3))

solve :: Sudoku -> Maybe Sudoku
solve s | not (isSudoku s) = Nothing
        | not (isOkay s)   = Nothing
        | isSolved s       = Just s
        | otherwise        = (listToMaybe . catMaybes) sols
  where
    nextEmpty = (head . blanks) s
    sols      = [solve (update s nextEmpty (Just i)) | i <- [1..9]]

readAndSolve :: FilePath -> IO Sudoku
readAndSolve f = readSudoku f >>= return . fromJust . solve

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2
  | isSolved s1 = s1 == fromJust (solve s2)
  | otherwise   = False

-- the solve function is sound if every solution produced is an actual solution
-- of the input
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isSudoku s && isJust s' ==> isSolutionOf (fromJust s') s
                    where s' = solve s

-------------------------------------------------------------------------
-- Finally, solve the competition problem

parseSudokus :: String -> [Sudoku]
parseSudokus = map parseSudoku . splitOn ["\r"] . lines
  where
    parseSudoku = Sudoku . map (map parseCell . words)
    parseCell   = fmap read . mfilter (all isDigit) . Just

solveChallenge :: IO ()
solveChallenge
   =  writeFile "ActualOutput.txt"
  =<< fromMaybe "No solutions found!!"
   .  fmap unlines
   .  mapM (fmap prettySudoku . solve)
   .  parseSudokus
  <$> readFile "ActualInput.txt"

testSamples :: IO ()
testSamples = do
  puzzles <- parseSudokus <$> readFile "SampleInput.txt"
  solns   <- parseSudokus <$> readFile "SampleSolution.txt"
  print puzzles
  print solns
--  let solved = mapM solve puzzles
  print (solve (head puzzles))
--  print $ solved == Just solns
