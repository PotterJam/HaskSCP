module Main where

import Lib
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Monoid
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

findFileToRequest :: FilePath -> Session -> IO String
findFileToRequest src s = do
  putStrLn $ "Looking in " ++ src ++ ":"
  let lsCmd = "ls " ++ src
  (_, (x:_)) <- execCommands s [lsCmd]
  TIO.putStrLn $ decodeUtf8 $ BSL.toStrict x
  putStrLn "grep: "
  toGrep <- getLine
  let grepFiles = lsCmd ++ " -p | grep -iv / | grep -i " ++ toGrep
      grepDirs  = "tree " ++ src ++ " -d -L 1 -i --noreport | grep -i " ++ toGrep
  (_, (y:_)) <- execCommands s [grepFiles]
  (_, (z:_)) <- execCommands s [grepDirs]
  let files        = indexFiles y
      dirs         = indexFiles z
  mapM_ putStrLn ["Files:", (unlines $ map snd files), "", "Dirs:", (unlines $ map snd dirs), ""]
  putStrLn "Please enter the f for files, or d for dirs, plus the index of what you would like to download."
  (c:i) <- getLine
  let index = read i :: Int
  case c of
    'f' -> return $ "/" ++ getFile index files
    'd' -> findFileToRequest (src ++ "/" ++ getFile index dirs) s
    _   -> error "Not selected file or dir"

getFile :: Int -> [(Int, String)] -> String
getFile index = removeIndex False . snd . retrieveFile . find (\(n, _) -> n == index)
  where
    retrieveFile Nothing  = error "Failed to retrieve file"
    retrieveFile (Just v) = v

indexFiles :: BSL.ByteString -> [(Int, String)]
indexFiles files = zipWith (\x n -> (n, show n ++ " " ++ T.unpack x)) decodedFiles [1..]
  where
    decodedFiles = T.lines $ decodeUtf8 $ BSL.toStrict files

removeIndex :: String -> String
removeIndex False (x:xs) = remInd (x == ' ') xs
removeIndex True str@(x:xs)
  | x /= ' '  = str
  | otherwise = remInd True xs
