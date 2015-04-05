{-# LANGUAGE ViewPatterns #-}

module Server where

import Data.List (stripPrefix)
import Data.Map (Map, empty, toList, insert, delete)
import Network.Socket

type Car = String
type Cars = Map SockAddr Car

status200 = "200 OK"
status400 = "400 Bad Request"

showCar (sockAddr, car) = unwords $ show sockAddr : [car]
showCars cars = unlines $ map showCar $ toList cars

process (stripPrefix "JOIN" -> Just car) sender cars = (insert sender car cars, status200)
process "QUIT" sender cars = (delete sender cars, status200)
process "LIST" sender cars = (cars, unlines $ status200 : [showCars cars])
process _ _ cars = (cars, status400)

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
