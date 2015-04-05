{-# LANGUAGE ViewPatterns #-}

module Server where

import Data.List (stripPrefix)
import Data.Map as Map (Map, empty, toList, insert, delete, filter)
import Network.Socket
import Data.Time.Clock
import Util

type Car = String
type CarInfo = (Car, UTCTime)
type Cars = Map SockAddr CarInfo

-- in second
expire = 64

status200 = "200 OK"
status400 = "400 Bad Request"

showCar (sockAddr, (car, _)) = unwords $ show sockAddr : [car]
showCars cars = unlines $ map showCar $ toList cars

process (stripPrefix "JOIN" -> Just car) sender now cars =
    (insert sender (car, now) cars, status200)
process "QUIT" sender _ cars = (delete sender cars, status200)
process "LIST" sender now cars =
    (onlineCars, unlines $ status200 : [showCars onlineCars])
  where
    onlineCars = Map.filter online cars
    online (_, when) = diffUTCTime now when < expire
process _ _ _ cars = (cars, status400)

loop :: Socket -> Cars -> IO ()
loop sock cars = do
    (command, _, sender) <- recvFrom sock 1024
    now <- getCurrentTime
    let (nextCars, response) = process (trim command) sender now cars
    sendTo sock response sender
    loop sock nextCars

server host port = withSocketsDo $ do
    sock <- socket AF_INET Datagram defaultProtocol
    addr <- inet_addr host
    bind sock $ SockAddrInet (toEnum $ read port) addr
    loop sock empty
