module Server where

import Network.Socket

server host port = withSocketsDo $ do
    sock <- socket AF_INET Datagram defaultProtocol
    addr <- inet_addr host
    bind sock $ SockAddrInet (PortNum $ read port) addr
    putStrLn $ "server " ++ host ++ ":" ++ port
