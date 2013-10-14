module Main where

import System.ZMQ3.Monadic
import Control.Monad (forever)
import Data.ByteString.Char8 (pack, unpack)
import Play

main :: IO ()
main =
    runZMQ $ do  
        liftIO $ putStrLn "Starting Server"
        repSocket <- socket Rep
        bind repSocket "tcp://*:5555"
  
        forever $ do
            msg <- receive repSocket
            (liftIO.putStrLn.unwords) ["Received request:\n", unpack msg]
            (liftIO.putStrLn.unwords) ["Write Response:\n", process(unpack msg) ]
            send repSocket [] (pack (process(unpack msg)))
