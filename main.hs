module Main where

import Data.Map (Map)
import Data.Bifunctor (bimap, second)
import Data.List (intercalate, sort, sortOn)
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down(..))
import qualified Data.Map as Map

type Pos = (Int, Int)
type Move = (Int, Int)
data Player = Black | White deriving (Eq)
data PieceType = Pawn | Queen deriving (Eq)
data Piece = Piece Player PieceType deriving (Eq)

instance Show Piece where
  show (Piece White Pawn) = "♙"
  show (Piece Black Pawn) = "♟"
  show (Piece White Queen) = "♕"
  show (Piece Black Queen) = "♛"

pieceType (Piece _ t) = t
pieceColor (Piece c _) = c
isWhite (Piece c _) = c == White
isBlack (Piece c _) = c == Black
otherColor White = Black
otherColor Black = White

infinity = 1/0
playerToScore :: Player -> Float
playerToScore White = infinity -- infinity
playerToScore Black = -infinity -- negative infinity

-- Priorities for sorting moves for more efficient alpha beta pruning
takePriority = 5
defaultPriority = 1
fastPriority = 2
promotePriority = 7

scorePieceType Queen = 8
scorePieceType Pawn  = 1

scorePiece (Piece Black x) = - scorePieceType x
scorePiece (Piece White x) = scorePieceType x

data Board = Board { board     :: Map Pos Piece
             , numWhite  :: Int -- number of pieces in their respective color
             , numBlack  :: Int
             , boardSize :: (Int, Int)
             } deriving (Eq)

instance Show Board where
  show board = "Board " ++ show (boardSize board) ++ " " ++ show (numWhite board) ++ " vs. " ++ show (numBlack board) ++ "\n" ++ intercalate ("\n" ++ rowSeparator ++ "\n") (showRow <$> reverse [0..height- 1])
          where showRow row = intercalate "│" $ (\i -> maybe " " show $ accessPos (i, row) board) <$> [0..width- 1]
                rowSeparator = intercalate "┼" $ replicate width "─"
                width  = fst $ boardSize board
                height = snd $ boardSize board

accessPos :: Pos -> Board -> Maybe Piece
accessPos x y = Map.lookup x (board y)

isTerminal :: Board -> Maybe Player
isTerminal b | numWhite b == 0 = Just Black
             | numBlack b == 0 = Just White
             | otherwise = Nothing

outsideBoard :: Pos -> Board -> Bool
outsideBoard pos b = uncurry (||) (applyTuple (\x y -> x < 0 ||x >= y) pos $ boardSize b)

insideBoard :: Pos -> Board -> Bool
insideBoard pos = not . outsideBoard pos

heuristicScore :: Board -> Float
heuristicScore b = case isTerminal b of 
  Just x -> playerToScore x
  Nothing -> Map.foldr (\a accum -> accum + scorePiece a) 0 (board b)

moveUncheckedPiece :: Maybe Piece -> Pos -> Pos -> Board -> Board
moveUncheckedPiece piece from_ to_ b = Board {
    board = newBoard,
    numWhite = newNumWhite,
    numBlack = newNumBlack,
    boardSize = boardSize b
  } where b_ = board b
          taken = Map.lookup to_ b_ -- if "to" contains something, it gets taken
          (newNumWhite, newNumBlack) = case taken of 
            Just (Piece White _) -> (numWhite b - 1, numBlack b)
            Just (Piece Black _) -> (numWhite b, numBlack b - 1)
            Nothing         -> (numWhite b, numBlack b)
          deleted = Map.delete from_ b_
          newBoard = maybe deleted (\piece -> Map.insert to_ piece deleted) piece

moveUnchecked :: Pos -> Pos -> Board -> Board
moveUnchecked from_ to_ b = moveUncheckedPiece (Map.lookup from_ $ board b) from_ to_ b

promotePawn :: Player -> Pos -> Pos -> Board -> Board
promotePawn player from_ to_ b = moveUncheckedPiece (Just $ Piece player Queen) from_ to_ b

applyTuple :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
applyTuple f a = bimap (f $ fst a) (f $ snd a)

addPos = applyTuple (+)

moveBoundsChecked :: Pos -> Pos -> Board -> Maybe Board
moveBoundsChecked from_ to_ b = if outsideBoard to_ b then Nothing else Just (moveUnchecked from_ to_ b)

moveDirection :: Pos -> Piece -> Board -> Move -> [(Int, Board)]
moveDirection p piece b m = moveDirection' (addPos p m) b
  where moveDirection' to_ b = if blocked then [] else
                                  case moveBoundsChecked p to_ b of
                                      Just x -> (if take then takePriority else defaultPriority, x) : (if take then [] else moveDirection' (addPos to_ m) b)
                                      Nothing -> []
                    where toPiece = accessPos to_ b
                          take = maybe False (\x -> pieceColor x /= pieceColor piece) toPiece
                          blocked = maybe False (\x -> pieceColor x == pieceColor piece) toPiece

possibleQueenMoves :: Pos -> Piece -> Board -> [(Int, Board)]
possibleQueenMoves pos piece b = concatMap (moveDirection pos piece b) directions
  where directions = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

maybeAdd :: Maybe a -> [a] -> [a]
maybeAdd Nothing = id
maybeAdd (Just x) = (x:)

possiblePawnMoves :: Pos -> Piece -> Board -> [(Int, Board)]
possiblePawnMoves pos piece b = a1 ++ takingBoards
  where y = snd pos
        height = snd $ boardSize b
        (canMoveTwice, isPromote1, isPromote2, direction) = if isWhite piece then (y < 2, y == height - 2, y == height - 3, 1) else (y > height - 3, y == 1, y == 2, -1)
        pos1 = addPos (0, direction) pos
        pos2 = addPos (0, 2 * direction) pos
        pos3 = addPos (1, direction) pos
        pos4 = addPos (-1, direction) pos
        a1 = if isNothing (accessPos pos1 b) && insideBoard pos1 b then
                (if isNothing (accessPos pos2 b) && canMoveTwice && insideBoard pos2 b then
                  [if isPromote2 then (promotePriority, promotePawn (pieceColor piece) pos pos2 b) else (fastPriority, moveUnchecked pos pos2 b)]
                else []) ++ [if isPromote1 then (promotePriority, promotePawn (pieceColor piece) pos pos1 b) else (defaultPriority, moveUnchecked pos pos1 b)]
              else []
        takingBoards = [if isPromote1 then (takePriority + promotePriority, promotePawn (pieceColor piece) pos i b) else (takePriority, moveUnchecked pos i b)| i <- [pos3, pos4], (pieceColor <$> accessPos i b) == Just (otherColor $ pieceColor piece)]

possibleMoves :: Board -> Player -> [Board]
possibleMoves b player = snd <$> (sortOn (Down . fst) $ Map.foldrWithKey (\a b c -> getMoves a b ++ c) [] filteredPieces)
  where filteredPieces = Map.filter ((== player) . pieceColor) (board b)
        getMoves pos piece = if pieceType piece == Pawn then possiblePawnMoves pos piece b else possibleQueenMoves pos piece b

maxBy1 x y = if fst x >= fst y then x else y
minBy1 x y = if fst x >= fst y then y else x

alphaBeta :: Board -> Int -> Float -> Float -> Bool -> (Float, [Board])
alphaBeta board depth ɑ β maximizingPlayer = if depth == 0 then (heuristicScore board, [])
  else maybe (runFun (possibleMoves board player)) (\x -> (playerToScore x, [board])) $ isTerminal board
  where player = if maximizingPlayer then White else Black
        maxFun :: [Board] -> (Float, [Board]) -> Float -> (Float, [Board])
        maxFun [] val newAlpha = val
        maxFun (x:xs) val ɑ
          | fst newVal >= β = newVal
          | otherwise = maxFun xs newVal (max ɑ $ fst newVal)
          where newVal = maxBy1 val $ (fst res, x : snd res)
                res = alphaBeta x (depth - 1) ɑ β False

        minFun :: [Board] -> (Float, [Board]) -> Float -> (Float, [Board])
        minFun [] val newBeta = val
        minFun (x:xs) val β
          | fst newVal <= ɑ = newVal
          | otherwise = minFun xs newVal (min β $ fst newVal)
          where newVal = minBy1 val $ (fst res, x : snd res)
                res = alphaBeta x (depth - 1) ɑ β True
        runFun x = if maximizingPlayer then maxFun x (-infinity, []) ɑ else minFun x (infinity, []) β 

evaluate board depth player = alphaBeta board depth (-infinity) infinity (player == White)


board1 = Board { numWhite = 1, numBlack = 1, boardSize = (8, 8), board = Map.fromList [((3, 3), Piece Black Queen) ] }
board2 = Board { numWhite = 1, numBlack = 1, boardSize = (8, 8), board = Map.fromList [((3, 3), Piece Black Queen), ((6, 3), Piece Black Pawn) ] }
board3 = Board { numWhite = 1, numBlack = 1, boardSize = (8, 8), board = Map.fromList [((3, 3), Piece Black Queen), ((6, 3), Piece White Pawn) ] }
board4 = Board { numWhite = 2, numBlack = 2, boardSize = (8, 8), board = Map.fromList [((2, 2), Piece White Pawn), ((3, 3), Piece Black Pawn), ((5, 5), Piece White Pawn), ((6, 6), Piece Black Pawn)] }
board5 = Board { numWhite = 1, numBlack = 1, boardSize = (8, 8), board = Map.fromList [((1, 6), Piece White Pawn), ((1, 1), Piece Black Pawn)] }

main = putStrLn "hi"

list = [1, 2]
[a, b, c] = list
