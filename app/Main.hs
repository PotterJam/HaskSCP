module Main where

import Lib
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.IO as TIO
import Network.SSH.Client.LibSSH2

main :: IO ()
main = do
  size <- testSSH2User testScp
  putStr $ show size

testSSH2User :: (Session -> IO a) -> IO a
testSSH2User = withSSH2User "/home/james/Scripts" "" "" "" 22

testScp :: Session -> IO Integer
testScp = scpReceive "" ""

scpReceive :: FilePath -> FilePath -> Session -> IO Integer
scpReceive src dest s = do
  requestPath <- findFileToRequest src s
  scpReceiveFile s requestPath dest

findFileToRequest :: FilePath -> Session -> IO String
findFileToRequest src s = do
  putStrLn $ "Looking in " ++ src ++ ":"
  let lsCmd = "ls " ++ src
  (_, (x:_)) <- execCommands s [lsCmd]
  TIO.putStrLn $ decodeUtf8 x
  putStrLn "grep: "
  toGrep <- getLine
  let grepCmd = "grep -i " ++ toGrep
  (_, (y:_)) <- execCommands s [lsCmd ++ " | " ++ grepCmd]
  TIO.putStrLn $ decodeUtf8 y
  putStrLn "Please enter what you would like to download."
  fileName <- getLine
  return $ src ++ "/" ++ fileName

