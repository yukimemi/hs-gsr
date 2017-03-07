module Main where

import Control.Exception.Safe
import Control.Monad
import Debug.Trace
import Lib
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Process

data Options = Options { showv :: Bool,
                         showStatus :: Bool,
                         inDir :: FilePath } deriving Show

data Status = Status { path :: FilePath,
                       diff :: Bool,
                       status :: String } deriving Show

type StatusList = [Status]

version = "0.1.0"

showvP :: Parser Bool
showvP = switch $ long "version" <> short 'v' <> help "Show version"

statusP :: Parser Bool
statusP = switch $ long "status" <> help "Show git status"

inDirP :: Parser FilePath
inDirP = argument str $ help "Target directory" <> metavar "DIR" <> value []

optP :: Parser Options
optP = Options <$> showvP <*> statusP <*> inDirP

opts :: ParserInfo Options
opts = helperInfo $ fullDesc <> progDesc "git status recurse under the DIR" <> header "git status recurse"
  where helperInfo = info $ helper <*> optP

ghqRoot :: IO FilePath
ghqRoot = do
  stdOut <- readProcess "ghq" ["root"] ""
  return $ chomp stdOut
  `catch` \(SomeException e) -> return "."

listDirRecurse :: FilePath -> IO [FilePath]
listDirRecurse path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      dirs <- listDirOnly path
      subs <- mapM listDirRecurse dirs
      return $ dirs ++ concat subs
    else return []

listDirOnly :: FilePath -> IO [FilePath]
listDirOnly path = do
  dirs <- listDirectory path
  let fulls = map (path </>) dirs
  filterDir fulls

filterDir :: [FilePath] -> IO [FilePath]
filterDir =  filterM doesDirectoryExist

isGitDir :: FilePath -> Bool
isGitDir path = case takeFileName path of
  ".git" -> True
  _ -> False

listGitDir :: [FilePath] -> IO [FilePath]
listGitDir = return . filter isGitDir

gitStatus :: FilePath -> IO Status
gitStatus path = do
  pwd <- getCurrentDirectory
  let root = takeDirectory path
  setCurrentDirectory root
  (code, _, _) <- readProcessWithExitCode "git" ["diff", "--quiet"] ""
  case code of
    ExitSuccess -> do
      setCurrentDirectory pwd
      return $ Status root False ""
    _ -> do
      stdOut <- readProcess "git" ["status"] ""
      setCurrentDirectory pwd
      return $ Status root True stdOut

gsr :: Options -> IO ()
gsr (Options True _ _) = putStrLn version
gsr (Options _ showStatus dir) = do
  dirs <- listDirRecurse =<< d
  gitDirs <- listGitDir dirs
  stList <- mapM gitStatus gitDirs
  forM_ stList $ \(Status p d s) -> do
    when d $ putStrLn p
    when showStatus $ putStrLn s
  where d = case dir of
                 [] -> ghqRoot
                 _ -> return dir

main :: IO ()
main = execParser opts >>= gsr

