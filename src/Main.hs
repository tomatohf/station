import System.Environment
import Text.Printf
import Server

main = getArgs >>= run

run (host:port:_) = server host port
run _ = do
    progName <- getProgName
    printf "usage: %s HOST PORT\n" progName
