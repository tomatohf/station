module Server where

import Data.Map
import Network.Socket

type Car = String
type Cars = Map SockAddr Car

process command sender cars = (cars, "999 UNKNOWN")

loop :: Socket -> Cars -> IO ()
loop sock cars = do
    (command, _, sender) <- recvFrom sock 1024
    let (nextCars, response) = process command sender cars
    sendTo sock response sender
    loop sock nextCars

server host port = withSocketsDo $ do
    sock <- socket AF_INET Datagram defaultProtocol
    addr <- inet_addr host
    bind sock $ SockAddrInet (toEnum $ read port) addr
    loop sock empty
