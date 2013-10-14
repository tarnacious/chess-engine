module Play where
import Data.Char
import Board
import Moves
import Parser
import Extend

process::String -> String
process input = case parseState input of
    Left _ -> "Parse Error\n"
    Right (c, b, m, []) -> (prettyState (c, b, m)) ++ (unwords $ [prettyMove m|m<-genColorMoves (c, b)]) ++ "\n"
    Right (c, b, m, [command]) ->
        case elem command ([m|m<-genColorMoves (c, b)]) of 
            True -> prettyState (makeMove c b command)  ++ "\n"
            False -> "Invalid Move\n"
