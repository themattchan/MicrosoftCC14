-- Following the programming exercise/tutorial from Chalmers University:
-- http://www.cse.chalmers.se/edu/year/2010/course/TDA451_Functional_Programming/labs/3/

module Sudoku where
import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
              deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

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

-- isSudoku sud checks if s is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = length (rows s) == 9 && -- s has 9 rows
             and [ length r == 9 &&
                   and [ isValid  x | x <- r]
                       | r <- rows s]
                 where
                   isValid :: Maybe Int -> Bool
                   isValid Nothing  = True
                   isValid (Just n) = n >= 1 && n <= 9

-- isSolved sud checks if s is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved (Sudoku s) = Nothing `notElem` (concat s)

-- printSudoku sud prints a representation of the sudoku s on the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku s) = print $ unlines $ (map (map printCell) s)
    where
      printCell Nothing  = '.'
      printCell (Just n) = (head . show) n -- show n is a string, head is a char

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  return $ Sudoku (map parseRow (lines f))
    where
      parseRow xs = map parseChar xs
      parseChar x
          | x == '.'             = Nothing
          | x `elem` ['1'..'9'] = Just (digitToInt x)
          | otherwise           = error "not a sudoku puzzle"

-------------------------------------------------------------------------
-- Testing. Skip this for now
{-
-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)
-}
-------------------------------------------------------------------------

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock xs = let ys = [n | n<-xs, n/=Nothing] in
                 length ys == length (nub ys)

blocks :: Sudoku -> [Block]
blocks (Sudoku s) = nineSquares s ++ transpose s ++ s

blocksSet :: Sudoku -> ([Block],[Block],[Block])
blocksSet (Sudoku s) = (s, transpose s, nineSquares s)

nineSquares s = concat [threeRows s row | row <- [3,6,9]]
    where
      threeRows s row = [concat [drop (x-3) (take x y)
                                 | y <- drop (row-3) (take row s)]
                         | x <- [3,6,9]]
isOkay :: Sudoku -> Bool
isOkay s = and [isOkayBlock b | b <- blocks s]

type Pos = (Int,Int)
    --deriving (Ord, Eq)

indexedPuzzle :: Sudoku -> [(Int, [(Int, Maybe Int)])]
indexedPuzzle (Sudoku s) = zip [0..8] (map (zip [0..8]) s)

blanks :: Sudoku -> [Pos]
blanks s = toPosList $ getBlanks $ indexedPuzzle s
    where
      getBlanksRow :: [(Int, Maybe Int)] -> [Int]
      getBlanksRow [] = []
      getBlanksRow ((c,x):xs)  = case x of
                            Nothing -> c : getBlanksRow xs
                            Just _   -> getBlanksRow xs
      getBlanks :: [(Int,[(Int,Maybe Int)])] -> [(Int, [Int])]
      getBlanks [] = []
      getBlanks ((r,xs):rs) = (r, getBlanksRow xs) : getBlanks rs
      toPosList :: [(Int, [Int])] -> [Pos]
      toPosList [] = []
      toPosList ((r, cs):rs) = map (\c -> (r,c)) cs ++ toPosList rs

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _ = []
(!!=) (x:xs) (i,v)
    | i==0 = v:xs
    | otherwise = x: (xs !!= (i-1,v))

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku s) (r,c) i =
    Sudoku $ take r s ++ [(s!!r) !!= (c,i)] ++ drop (r+1) s

candidates :: Sudoku -> Pos -> [Int]
candidates (Sudoku s) (r,c) =
    [1..9] \\ ( map (\x ->  fromMaybe 0 x) (rowCands ++ colCands ++ boxCands))
    where
      (sr,sc,sb) = blocksSet (Sudoku s)
      rowCands = sr !! r
      colCands = sc !! c
      boxCands = sb !! ((r `div` 3) *3 + c `div` 3)

--solve :: Sudoku -> Maybe Sudoku
solve s
    | not (isSudoku s) = Nothing
    | not (isOkay s) = Nothing
    | isSolved s   = Just s
    | otherwise    = case filter (/=Nothing) sols of
                       [] -> Nothing
                       (x:_) -> x
    where
      nextEmpty = head $ blanks s
      sols = [solve (update s nextEmpty (Just i)) | i <-[1..9]]

readAndSolve :: FilePath -> IO Sudoku
readAndSolve f = do
  puzzle <- readSudoku f
  return $ fromJust $ solve puzzle

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2
      | isSolved s1 = s1 == fromJust (solve s2)
      | otherwise = False

-- the solve function is sound if every solution produced is an actual solution
-- of the input
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isSudoku s && isJust s' ==> isSolutionOf (fromJust s') s
                    where s' = solve s

-------------------------------------------------------------------------
-- Finally, solve the competition problem

-- readChallenge f = do
--   s <- readFile "ActualInput.txt"
--   let ss = map init $ chunksOf 10 (map (init . (filter (/=' '))) (lines f)) in
--   return $ map show result
--     where
--       result = map fromJust $ map solve $ map Sudoku $ map (map $ map parseChar) ss
--       parseChar x
--           | x == 'x'             = Nothing
--           | x `elem` ['1'..'9'] = Just (digitToInt x)
--           | otherwise           = error "not a sudoku puzzle"
