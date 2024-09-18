module Main where

import Data.Map (Map)
import qualified Data.Map as Map

type Pos = (Int, Int)
data Player = Black | White deriving (Eq, Show)
data PieceType = Pawn | Queen deriving (Eq, Show)
type Piece = (Player, PieceType)
data Board = Board { board     :: Map Pos Piece
             , numWhite  :: Int -- number of pieces in their respective color
             , numBlack  :: Int
             , boardSize :: (Int, Int)
             } deriving (Eq, Show)

isTerminal :: Board -> Maybe Player
isTerminal b | numWhite b == 0 = Just Black
             | numBlack b == 0 = Just White
             | otherwise = Nothing

playerToScore :: Player -> Float
playerToScore White = 1 / 0 -- infinity
playerToScore Black = -1 / 0 -- negative infinity

moveUnchecked :: Board -> Pos -> Pos -> Board
moveUnchecked b from_ to_ = Board {
    board = board b,
    numWhite = newNumWhite,
    numBlack = newNumBlack,
    boardSize = boardSize b
  }
    where b_ = board b
          taken = Map.lookup to_ b_ -- if "to" contains something, it gets taken
          (newNumWhite, newNumBlack) = case taken of 
            Just (White, _) -> (numWhite b - 1, numBlack b)
            Just (Black, _) -> (numWhite b, numBlack b - 1)
            Nothing -> (numWhite b, numBlack b)


possiblePawnMoves :: Pos -> Board -> [Board]
possiblePawnMoves pos b = undefined
  where b_ = board b

possibleMoves :: Board -> Player -> [Board]
possibleMoves b player = undefined
  where filteredPieces = Map.filter (\(a, b) -> a == player) (board b)

-- alphaBeta board depth alpha beta maxplayer =
--  maybe undefined playerToScore $ isTerminal board
















main = putStrLn "hi"
