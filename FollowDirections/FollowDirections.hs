{-# LANGUAGE TypeSynonymInstances #-}
module FollowDirections where

import Control.Monad
import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Token

--import

data Instr = Step Int | Turn Direction
data Direction = Left | Right


data Orientation = N | E | S | W
               deriving (Eq, Ord, Show, Read, Enum)



type Coord = (Int, Int)
data MyState = St Coord Orientation

instrP :: Parser Instr
instrP = movP <|> turnP

movP :: Parser Instr
movP =

parseInstr :: String -> Either ParseError [Instr]
parseInstr = undefined

turnLeft (St _ d) = case d of
  N -> W
  _ -> pred d

turnRight (St _ d) = case d of
  W -> N
  _ -> succ d


-- move (St (x,y) N)
-- move (St (x,y) E)
-- move (St (x,y) S)
-- move (St (x,y) W)
