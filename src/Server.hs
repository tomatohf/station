module Server where

import Network.Socket

server host port = withSocketsDo $ do
    putStrLn $ "server " ++ host ++ ":" ++ port
