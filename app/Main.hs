module Main where

import System.Process     ( readProcessWithExitCode )
import System.Environment ( getArgs )
import System.Hclip       ( setClipboard )
import GHC.IO.Exception   ( ExitCode (..) )

type Argument = String
type Stdin    = String
type Stdout   = String
type Stderr   = String
type Code     = String

ykman :: [Argument] -> Stdin -> IO (ExitCode, Stdout, Stderr)
ykman = readProcessWithExitCode "ykman"

oath :: Argument -> IO (Either Stderr Code)
oath query = do
    result <- resultToEither <$> ykman ["oath", "accounts", "code", "-s", query] ""
    return $ takeWhile (/= '\n') <$> result

resultToEither :: (ExitCode, b, a) -> Either a b
resultToEither result = case result of
                (ExitSuccess,s,_) -> Right s
                (_,_,e)           -> Left e

usage :: String
usage = "+--------------+-------+\n" ++
        "| ykman-helper | USAGE |\n" ++
        "+--------------+-------+\n" ++
        "    ykman-helper [-c | -cc] account_name\n"

printUsage :: IO ()
printUsage = putStrLn usage

printError :: String -> IO ()
printError = putStr

printCode :: String -> IO ()
printCode = putStrLn

oathOut :: Argument -> IO ()
oathOut query = oath query >>= either printError printCode

oathOutClip :: Argument -> IO ()
oathOutClip query = oath query >>= either printError handleCode
    where 
        handleCode :: Code -> IO ()
        handleCode c = setClipboard c >> putStrLn c

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-c", query]  -> oathOut query
        ["-cc", query] -> oathOutClip query
        ["-h"]         -> printUsage
        _              -> printUsage
