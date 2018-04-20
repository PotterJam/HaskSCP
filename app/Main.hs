module Main where

import Lib
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Monoid
import Control.Monad
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
  lsOutput <- execCmd s lsCmd
  TIO.putStrLn lsOutput
  putStrLn "grep: "
  toGrep <- getLine
  let grepFiles = lsCmd ++ " -p | grep -iv / | grep -i " ++ toGrep
      grepDirs  = "tree " ++ src ++ " -d -L 1 -i --noreport | grep -i " ++ toGrep
  files <- liftM indexFiles $ execCmd s grepFiles
  dirs <- liftM indexFiles $ execCmd s grepDirs
  mapM_ putStrLn ["Files:", (unlines $ map snd files), "", "Dirs:", (unlines $ map snd dirs), ""]
  putStrLn "Please enter the f for files, or d for dirs, plus the index of what you would like to download."
  (c:i) <- getLine
  let index = read i :: Int
  case c of
    'f' -> return $ "/" ++ getFile index files
    'd' -> findFileToRequest (src ++ "/" ++ getFile index dirs) s
    _   -> error "Not selected file or dir"

execCmd :: Session -> String -> IO T.Text
execCmd s cmd = do
  (_, (x:_)) <- execCommands s [cmd]
  return $ decodeUtf8 $ BSL.toStrict x

getFile :: Int -> [(Int, String)] -> String
getFile index = removeIndex False . snd . retrieveFile . find (\(n, _) -> n == index)
  where
    retrieveFile Nothing  = error "Failed to retrieve file"
    retrieveFile (Just v) = v

indexFiles :: T.Text -> [(Int, String)]
indexFiles f = zipWith (\x n -> (n, show n ++ " " ++ T.unpack x)) (T.lines f) [1..]

removeIndex :: Bool -> String -> String
removeIndex False (x:xs) = removeIndex (x == ' ') xs
removeIndex True str@(x:xs)
  | x /= ' '  = str
  | otherwise = removeIndex True xs
