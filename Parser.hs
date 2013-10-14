module Parser where

import Text.ParserCombinators.Parsec
import Board
import Data.Char  
import Minimax
import Moves
import Extend

king :: Parser PieceType
king = do
    char 'K'
    return King

queen :: Parser PieceType
queen = do
    char 'Q'
    return Queen

bishop :: Parser PieceType
bishop = do
    char 'B'
    return Bishop

knight :: Parser PieceType
knight = do
    char 'N'
    return Knight

rook :: Parser PieceType
rook = do
    char 'R'
    return Rook

pawn :: Parser PieceType
pawn = do
    char 'P'
    return Pawn

black :: Parser PieceColor
black = do
    char 'B'
    return Black

white :: Parser PieceColor
white = do
    char 'W'
    return White

string' :: String -> Parser String
string' "" = return "" 
string' (x:xs) = do
    char x
    string' xs
    return (x:xs)

blackTurn :: Parser PieceColor
blackTurn = do
    spaces
    string "Black"
    spaces
    return Black

whiteTurn :: Parser PieceColor
whiteTurn = do
    spaces
    string "White"
    spaces
    return White

theTurn :: Parser PieceColor
theTurn = whiteTurn <|> blackTurn

pieceType :: Parser PieceType
pieceType = king <|> queen <|> bishop <|> knight <|> rook <|> pawn 

pieceColour :: Parser PieceColor
pieceColour = black <|> white

emptySquare:: Parser Square
emptySquare = do
    char '-'
    char '-'
    return Nothing

boardPiece :: Parser Square
boardPiece = do
    p <- piece
    return $ Just p

square:: Parser Square
square = boardPiece <|> Parser.emptySquare

piece :: Parser Piece
piece = do
    colour <- pieceColour
    ptype <- Parser.pieceType
    return $ Piece ptype colour

parseState :: String -> Either ParseError (PieceColor, [[Square]], [(Piece, Pos, Pos)], [(Piece, Pos, Pos)])
parseState input = parse state' "" input

row :: Parser [Square]
row = count 8 boardSquare

board :: Parser [[Square]] 
board = count 8 row

state' :: Parser (PieceColor, [[Square]], [(Piece, Pos, Pos)],[(Piece, Pos, Pos)])
state' = do
    t <- theTurn
    b <- board 
    m <- movesParser
    c <- many commandParser
    return $ (t, b, m, c)

movesParser :: Parser [(Piece, Pos, Pos)] 
movesParser = many moveParser 

boardSquare :: Parser Square
boardSquare = do
    spaces
    s1 <- square
    spaces
    return s1

colParser :: Parser Char
colParser = char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8'

rowParser :: Parser Char
rowParser = char 'a' <|> char 'b' <|> char 'c' <|> char 'd' <|> char 'e' <|> char 'f' <|> char 'g' <|> char 'h'

posParser :: Parser Pos
posParser = do
    r <- rowParser
    c <- colParser
    return (toPos (r:[c]))

commandParser :: Parser (Piece, Pos, Pos)
commandParser = do
    spaces
    c <- string' "action:" 
    spaces
    m <- moveParser 
    return m
 

moveParser :: Parser (Piece, Pos, Pos)
moveParser = do
    spaces
    p <- piece
    spaces
    p1 <- posParser
    char ':'
    p2 <- posParser
    spaces
    return (p, p1, p2)
