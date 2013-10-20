module Play where
import Data.Char
import Board
import Moves
import Parser
import Extend
import Minimax
import Evaluator

process::String -> String
process input = case parseState input of
    Left _ -> "Parse Error\n"

    Right (c, b, m, []) -> case result (c, b) of
        Just r -> r ++ "\n" ++ (Board.prettyBoard b) ++ "\n"
        Nothing -> case genLegalMoves (c, b) of
            [] -> case isCheckmate c b of
                True -> case c of
                    White -> "Black Wins\n" ++ Board.prettyBoard b
                    Black -> "White Wins\n" ++ Board.prettyBoard b
                False -> "Statemate\n" ++ Board.prettyBoard b
            moves ->Extend.prettyState (c, b, moves)

    Right (c, b, m, [command]) -> case result (c, b) of
        Just r -> r ++ "\n" ++ (Board.prettyBoard b) ++ "\n"
        Nothing -> Play.makeMove c b command


result::State -> Maybe String
result st | sw > threshold  = Just "White wins"
          | sw < -threshold = Just "Black wins"
          | otherwise = Nothing
    where sw = evalState st

makeMove::PieceColor -> Board -> Move -> String
makeMove c b command = case elem command ([m|m<-genLegalMoves (c, b)]) of
    True -> case Extend.makeMove c b command of
        (c', b') -> case genLegalMoves (c', b') of
            [] -> case isCheckmate c b of
                True -> case c of
                    White -> "Black Wins\n" ++ Board.prettyBoard b
                    Black -> "White Wins\n" ++ Board.prettyBoard b
                False -> "Statemate\n" ++ Board.prettyBoard b
            moves -> (Extend.prettyState (c', b', moves)) ++ "\n"
    False -> "Invalid Move\n"
