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
veryBig = 10000000
playerToScore :: Player -> Float
playerToScore White = veryBig
playerToScore Black = -veryBig

-- Priorities for sorting moves for more efficient alpha beta pruning
takePriority = 5
defaultPriority = 1
fastPriority = 2
promotePriority = 7
penaltyStep = 0.01

scorePieceType Queen = 8
scorePieceType Pawn  = 1

scorePiece (Piece Black x) = - scorePieceType x
scorePiece (Piece White x) = scorePieceType x

data Board = Board { board     :: Map Pos Piece
             , numWhite  :: Int -- number of pieces in their respective color
             , numBlack  :: Int
             , boardSize :: (Int, Int)
             , enPassantable :: Maybe Pos
             } deriving (Eq)

instance Show Board where
  show b = "Board " ++ show (boardSize b) ++ " " ++ show (numWhite b) ++ " vs. " ++ show (numBlack b) ++ ", outside: " ++ show outsidePieces ++ ", en passantable: " ++ show (enPassantable b) ++ "\n" ++ intercalate ("\n" ++ rowSeparator ++ "\n") (showRow <$> reverse [0..height- 1])
          where showRow row = intercalate "│" $ (\i -> maybe " " show $ accessPos (i, row) b) <$> [0..width- 1]
                rowSeparator = intercalate "┼" $ replicate width "─"
                width  = fst $ boardSize b
                height = snd $ boardSize b
                outsidePieces = Map.filterWithKey (\x _ -> outsideBoard x b) (board b)

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
moveUncheckedPiece piece from_ to_ b =
    Board { board = newBoard
       , numWhite = newNumWhite
       , numBlack = newNumBlack
       , boardSize = boardSize b
       , enPassantable = Nothing
       }  where b_ = board b
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
promotePawn player = moveUncheckedPiece (Just $ Piece player Queen)

removePiece :: Pos -> Board -> Board
removePiece pos = moveUncheckedPiece Nothing pos pos

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

canTake :: Board -> Piece -> Pos -> Bool
canTake b piece pos = (pieceColor <$> accessPos pos b) == Just (otherColor $ pieceColor piece)

freeSquare :: Board -> Pos -> Bool
freeSquare b p = isNothing (accessPos p b)

possiblePawnMoves :: Pos -> Piece -> Board -> [(Int, Board)]
possiblePawnMoves pos piece b = movingForward ++ takingBoards
  where y = snd pos
        height = snd $ boardSize b
        (canMoveTwice, isPromote1, isPromote2, direction) = if isWhite piece then (y < 2, y == height - 2, y == height - 3, 1) else (y > height - 3, y == 1, y == 2, -1)
        pos1 = addPos (0, direction) pos
        pos2 = addPos (0, 2 * direction) pos
        pos3 = addPos (1, direction) pos
        pos4 = addPos (-1, direction) pos
        maybePromote promote from_ to_ b prio = if promote then (prio + promotePriority, promotePawn (pieceColor piece) from_ to_ b) else (prio, moveUnchecked from_ to_ b)
        movingForward = if freeSquare b pos1 && insideBoard pos1 b then
                          (if freeSquare b pos2 && canMoveTwice && insideBoard pos2 b then
                            [second (\x -> x {enPassantable = Just pos2}) (maybePromote isPromote2 pos pos2 b fastPriority) ]
                          else []) ++ [maybePromote isPromote1 pos pos1 b defaultPriority]
                        else []
        takingBoards = [second (if enPassanting then removePiece next_ else id) $ maybePromote isPromote1 pos to_ b takePriority
                          | offset <- [-1, 1],
                            let to_ = addPos (offset, direction) pos,
                            let next_ = addPos (offset, 0) pos,
                            let enPassanting = enPassantable b == Just next_,
                            canTake b piece to_ || enPassanting]

possibleMoves :: Board -> Player -> [Board]
possibleMoves b player = snd <$> sortOn (Down . fst) (Map.foldrWithKey (\a b c -> getMoves a b ++ c) [] filteredPieces)
  where filteredPieces = Map.filter ((== player) . pieceColor) (board b)
        getMoves pos piece = if pieceType piece == Pawn then possiblePawnMoves pos piece b else possibleQueenMoves pos piece b

maxBy1 x y = if fst x >= fst y then x else y
minBy1 x y = if fst x >= fst y then y else x

alphaBeta :: Board -> Int -> Float -> Float -> Float -> Bool -> (Float, [Board])
alphaBeta board depth depthPenalty ɑ β maximizingPlayer  = if depth == 0 then (heuristicScore board - depthPenalty, [])
  else maybe (runFun (possibleMoves board player)) (\x -> (playerToScore x - depthPenalty, [board])) $ isTerminal board
  where player = if maximizingPlayer then White else Black
        maxFun :: [Board] -> (Float, [Board]) -> Float -> (Float, [Board])
        maxFun [] val newAlpha = val
        maxFun (x:xs) val ɑ
          | fst newVal >= β = newVal
          | otherwise = maxFun xs newVal (max ɑ $ fst newVal)
          where newVal = maxBy1 val $ second (x:) res
                res = alphaBeta x (depth - 1) (depthPenalty + penaltyStep) ɑ β False 

        minFun :: [Board] -> (Float, [Board]) -> Float -> (Float, [Board])
        minFun [] val newBeta = val
        minFun (x:xs) val β
          | fst newVal <= ɑ = newVal
          | otherwise = minFun xs newVal (min β $ fst newVal)
          where newVal = minBy1 val $ second (x:) res
                res = alphaBeta x (depth - 1) (depthPenalty + penaltyStep) ɑ β True 
        runFun x = if maximizingPlayer then maxFun x (-infinity, []) ɑ else minFun x (infinity, []) β 

evaluate board depth player = alphaBeta board depth 0.0 (-infinity) infinity (player == White)

createBoard :: [(Int, Int, Player, PieceType)] -> Maybe Pos -> Board
createBoard x p = Board { numWhite = numWhite'
                        , numBlack = numBlack'
                        , enPassantable = p
                        , boardSize = (8, 8)
                        , board = Map.fromList $ (\(x, y, c, t) -> ((x, y), Piece c t)) <$> x
                        } where numWhite' = length $ filter (\(a, b, c, d) -> c == White) x
                                numBlack' = length x - numWhite'

board1 = createBoard [(3, 3, Black, Queen)] Nothing
board2 = createBoard [(3, 3, Black, Queen), (6, 3, Black, Pawn) ] Nothing
board3 = createBoard [(3, 3, Black, Queen), (6, 3, White, Pawn) ] Nothing
board4 = createBoard [(2, 2, White, Pawn), (3, 3, Black, Pawn), (5, 5, White, Pawn), (6, 6, Black, Pawn)] Nothing
board5 = createBoard [(1, 6, White, Pawn), (1, 1, Black, Pawn)] Nothing
board6 = createBoard [(1, 4, White, Pawn), (2, 4, Black, Pawn)] (Just (2, 4))

main = putStrLn "hi"
