module Extend where
import Data.Char
import Board
import Moves

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

makeMove::PieceColor -> Board -> Move -> (PieceColor, Board, [Move])
makeMove c b (v,from,to) =
    ((oppositeColor c), movePos from to b, genColorMoves ((oppositeColor c), (movePos from to b)))

prettyTurn :: PieceColor -> String
prettyTurn White = "White"
prettyTurn Black = "Black"

prettyState :: (PieceColor, Board, [Move]) -> String
prettyState (c, b, m) = prettyTurn c ++  "\n"  ++  Extend.prettyBoard b ++ "\n" ++ (unwords $ [prettyMove m'|m'<-m]) ++ "\n"

prettyMove::Move -> String
prettyMove (piece, pos1, pos2) = (prettyPiece piece) ++ (prettyPos pos1) ++ ":" ++ (prettyPos pos2)




