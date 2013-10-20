module Extend where
import Data.Char
import Board
import Moves
import Minimax
import Evaluator

prettyPiece::Piece->String
prettyPiece (Piece a f) = show f ++ show a ++ " "

prettySquare::Square->String
prettySquare Nothing = "-- "
prettySquare (Just p) = prettyPiece p

prettyBoard::Board->String
prettyBoard  = unlines . map (concatMap Extend.prettySquare)

prettyPos::Pos->String
prettyPos (x, y) = [chr ((ord 'a') + y)] ++ [chr((ord '1') + (7 - x))]

allSquarePos::Board->[(Square,Pos)]
allSquarePos board = [(getSquare board (a, b), (a, b))|a<-[0..7],b<-[0..7]]

piecePos::(Square,Pos)->[(Piece,Pos)]
piecePos (Nothing,_) = []
piecePos (Just pp, p) = [(pp,p)]

allPieces::[(Square,Pos)]->[[(Piece,Pos)]]
allPieces [] = []
allPieces (x:xs) = (piecePos x):(allPieces xs)

allPieces' :: [[(Piece,Pos)]]->[(Piece,Pos)]
allPieces' l = concat l

pieces ::  Board->[(Piece,Pos)]
pieces board = allPieces' (allPieces (allSquarePos board))

colorPieces :: PieceColor->Board->[(Piece,Pos)]
colorPieces color board = [((Piece ptype pcolor),pos)|((Piece ptype pcolor),pos)<-pieces board,color==pcolor]

genColorMoves::State->[(Piece,Pos,Pos)]
genColorMoves (c, b) = concat [[(piece,pos,z)|z<-genPieceMoves b pos piece]|(piece,pos)<-colorPieces c b]

type Move = (Piece, Pos, Pos)

makeMove::PieceColor -> Board -> Move -> (PieceColor, Board)
makeMove c b (v,from,to) =
    ((oppositeColor c), movePos from to b)

genLegalMoves :: (PieceColor, Board) -> [Move]
genLegalMoves (c, b) = filter (isLegalMove c b) (genColorMoves (c, b))

isLegalMove :: PieceColor -> Board -> Move -> Bool
isLegalMove c b m = case Extend.makeMove c b m of
    (c', b') -> (canCaptureKing c' b' (genColorMoves (c', b'))) == False

canCaptureKing :: PieceColor -> Board -> [Move] -> Bool
canCaptureKing c b [] = False
canCaptureKing c b (m:ms) = case capturedKing c b m of
    True -> True
    False -> canCaptureKing c b ms

capturedKing :: PieceColor -> Board -> Move -> Bool
capturedKing c b m = case Extend.makeMove c b m of
    (c', b') -> winningState c (c', b')

isCheckmate :: PieceColor -> Board -> Bool
isCheckmate c b = canCaptureKing (oppositeColor c) b (genColorMoves (oppositeColor(c), b))

prettyTurn :: PieceColor -> String
prettyTurn White = "White"
prettyTurn Black = "Black"

prettyState :: (PieceColor, Board, [Move]) -> String
prettyState (c, b, m) = prettyTurn c ++  "\n"  ++  Extend.prettyBoard b ++ "\n" ++ (unwords $ [prettyMove m'|m'<-m]) ++ "\n"

prettyMove::Move -> String
prettyMove (piece, pos1, pos2) = (prettyPiece piece) ++ (prettyPos pos1) ++ ":" ++ (prettyPos pos2)
