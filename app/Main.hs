module Main where

import Lib
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.List
import Network.SSH.Client.LibSSH2

main :: IO ()
main = do
  size <- testSSH2User testScp
  putStr $ show size



testSSH2User :: (Session -> IO a) -> IO a
testSSH2User = withSSH2User "" "" "" "" 22

testScp :: Session -> IO Integer
testScp = scpReceive "" ""

scpReceive :: FilePath -> FilePath -> Session -> IO Integer
scpReceive src dest s = do
  fileRequested <- findFileToRequest src s
  let srcPath  = src ++ fileRequested
      destPath = dest ++ fileRequested
  scpReceiveFile s srcPath destPath

-- mess but it works, refactor, make sure to use <> and stay with text all the way through
-- need IO Text type sig
findFileToRequest :: FilePath -> Session -> IO String
findFileToRequest src s = do
  putStrLn $ "Looking in " ++ src ++ ":"
  let lsCmd = "ls " ++ src
  (_, (x:_)) <- execCommands s [lsCmd]
  TIO.putStrLn $ decodeUtf8 x
  putStrLn "grep: "
  toGrep <- getLine
  (_, (y:_)) <- execCommands s [lsCmd ++ " | grep -i " ++ toGrep]
  let files        = T.lines $ decodeUtf8 y
      indexedFiles = zipWith (\x n -> (n, show n ++ " " ++ T.unpack x)) files [1..]
  putStrLn $ unlines (map snd indexedFiles)
  putStrLn "Please enter the index of what you would like to download."
  index <- getLine
  let selectedFile = getFile $ find (\(n, _) -> n == (read index)) indexedFiles
  return $ "/" ++ (drop 2 $ snd selectedFile)

getFile :: Maybe (a, b) -> (a, b)
getFile Nothing = error "Failed to retrieve file"
getFile (Just v) = v
