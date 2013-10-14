module Main where

import System.ZMQ3.Monadic
import Control.Monad (forM_)
import Data.ByteString.Char8 (pack, unpack)

main :: IO ()
main = do
    contents <- readFile "sample/make_move.txt"
    runZMQ $ do
        liftIO $ putStrLn "Connecting to Chess server…"  
        reqSocket <- socket Req
        connect reqSocket "tcp://localhost:5555"
        forM_ [1..10] $ \i -> do
            liftIO $ putStrLn $ unwords ["Sending request", contents]
            send reqSocket [] (pack contents)
            reply <- receive reqSocket
            liftIO $ putStrLn $ unwords ["Received reply:", unpack reply] 
