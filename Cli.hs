module Main where

import Play

main = do  
    contents <- getContents  
    putStr $ process contents 
