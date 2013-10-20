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
                            Nothing -> Play.genMoves c b m
    Right (c, b, m, [command]) -> Play.makeMove c b m command


result::State -> Maybe String
result st | sw > threshold  = Just "White wins"
            | sw < -threshold = Just "Black wins"
            | otherwise = Nothing
   where sw = evalState st

genMoves::PieceColor -> Board -> [Move] -> String
genMoves c b m = (prettyState (c, b, [])) ++ (unwords $ [prettyMove m|m<-genColorMoves (c, b)]) ++ "\n"

makeMove::PieceColor -> Board -> [Move] -> Move -> String
makeMove c b m command = case elem command ([m|m<-genColorMoves (c, b)]) of
    True -> (Extend.prettyState (Extend.makeMove c b command)) ++ "\n"
    False -> "Invalid Move\n"
