module Main where

import Data.Map (Map)
import Data.Bifunctor (bimap, second)
import qualified Data.Map as Map

type Pos = (Int, Int)
type Move = (Int, Int)
data Player = Black | White deriving (Eq, Show)
data PieceType = Pawn | Queen deriving (Eq, Show)
type Piece = (Player, PieceType)
data Board = Board { board     :: Map Pos Piece
             , numWhite  :: Int -- number of pieces in their respective color
             , numBlack  :: Int
             , boardSize :: (Int, Int)
             } deriving (Eq, Show)

accessPos :: Pos -> Board -> Maybe Piece
accessPos x y = Map.lookup x (board y)

isTerminal :: Board -> Maybe Player
isTerminal b | numWhite b == 0 = Just Black
             | numBlack b == 0 = Just White
             | otherwise = Nothing

playerToScore :: Player -> Float
playerToScore White = 1 / 0 -- infinity
playerToScore Black = -1 / 0 -- negative infinity

moveUnchecked :: Pos -> Pos -> Board -> Board
moveUnchecked from_ to_ b = Board {
    board = newBoard,
    numWhite = newNumWhite,
    numBlack = newNumBlack,
    boardSize = boardSize b
  }
    where b_ = board b
          taken = Map.lookup to_ b_ -- if "to" contains something, it gets taken
          (newNumWhite, newNumBlack) = case taken of 
            Just (White, _) -> (numWhite b - 1, numBlack b)
            Just (Black, _) -> (numWhite b, numBlack b - 1)
            Nothing         -> (numWhite b, numBlack b)
          deleted = Map.delete from_ b_
          newBoard = maybe deleted (\piece -> Map.insert to_ piece deleted) (Map.lookup from_ b_)

applyTuple :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
applyTuple f a = bimap (f $ fst a) (f $ snd a)

addPos = applyTuple (+)

moveBoundsChecked :: Pos -> Pos -> Board -> Maybe Board
moveBoundsChecked from_ to_ b = if uncurry (||) (applyTuple (\x y -> x < 0 ||x >= y) to_ $ boardSize b) then
    Nothing
  else Just (moveUnchecked from_ to_ b)

moveDirection :: Pos -> Piece -> Move -> Board -> [Board]
moveDirection p piece m b = moveDirection' (addPos p m) b
  where moveDirection' to_ b = if blocked then [] else
                                      case moveBoundsChecked p to_ b of
                                          Just x -> x : (if take then [] else moveDirection' (addPos to_ m) b)
                                          Nothing -> []
                    where toPiece = accessPos to_ b
                          take = maybe False (\x -> fst x /= fst piece) toPiece
                          blocked = maybe False (\x -> fst x == fst piece) toPiece

possibleQueenMoves :: Pos -> Board -> [Board]
possibleQueenMoves pos b = maybe [] (\piece -> concatMap (\m -> moveDirection pos piece m b) directions) (accessPos pos b)
  where directions = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

possibleMoves :: Board -> Player -> [Board]
possibleMoves b player = undefined
  where filteredPieces = Map.filter (\(a, b) -> a == player) (board b)

-- alphaBeta board depth alpha beta maxplayer =
--  maybe undefined playerToScore $ isTerminal board







board1 = Board { numWhite = 1, numBlack = 1, boardSize = (8, 8), board = Map.fromList [((3, 3), (Black, Queen)) ] }
board2 = Board { numWhite = 1, numBlack = 1, boardSize = (8, 8), board = Map.fromList [((3, 3), (Black, Queen)), ((6, 3), (Black, Pawn)) ] }
board3 = Board { numWhite = 1, numBlack = 1, boardSize = (8, 8), board = Map.fromList [((3, 3), (Black, Queen)), ((6, 3), (White, Pawn)) ] }



main = putStrLn "hi"
