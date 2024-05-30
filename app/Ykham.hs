module Main where

import System.Process         ( readProcessWithExitCode )
import System.Environment     ( getArgs )
import System.Hclip           ( setClipboard )

import Control.Monad.Except
import Control.Monad.IO.Class ( liftIO )
import GHC.IO.Exception       ( ExitCode (ExitSuccess) )

import Options.Applicative
import qualified Options.Applicative as Opt

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

--------------------------------------------------------------------
-- ykman
--------------------------------------------------------------------

type YkmanM = ExceptT Text IO

type Argument = Text
type Query    = Text
type Stdin    = Text
type Code     = Text

type Issuer   = Text
type Username = Text

data Entry 
    = MinE Username
    | FullE Issuer Username
    deriving (Eq, Show)

createEntry :: Text -> Entry
createEntry input = 
    if snd spanned == T.empty
        then MinE input
        else FullE issuer username
    where
        spanned  = T.span (/= ':') input
        issuer   = fst spanned
        username = T.tail $ snd spanned

ykman :: [Argument] -> Stdin -> YkmanM Text
ykman args stdin = do
    result@(_,s,e) <- liftIO $ readProcessWithExitCode "ykman" (map T.unpack args) (T.unpack stdin)
    if isSuccess result 
        then pure $ T.pack s
        else throwError $ T.pack e

    where
        isSuccess (ExitSuccess,s,_) = not $ null s
        isSuccess (_,_,e)           = False

readOath :: Query -> YkmanM Code
readOath q = head . T.lines <$> ykman ["oath", "accounts", "code", "-s", q] ""  
          -- ^^^^ safe usage of ´head´ because of ´null´ check in ´isSuccess´
      
listOath :: YkmanM (Map Int Entry)
listOath = M.fromList <$> indexedEntries
    where
        indexedEntries = index . map createEntry . T.lines <$> ykman ["oath", "accounts", "list"] ""

--------------------------------------------------------------------
-- HANDLING RESULTS / UTILS
--------------------------------------------------------------------

printError :: Code -> IO ()
printError e = TIO.putStrLn $ "ERROR (ykman-helper): " <> e

printCode :: Text -> IO ()
printCode = TIO.putStrLn

clipPrintCode :: Code -> IO ()
clipPrintCode c = setClipboard (T.unpack c) >> printCode c

handleResult :: (a -> IO ()) -> Either Text a -> IO ()
handleResult = either print

index :: [a] -> [(Int,a)]
index = zip [1..]

--------------------------------------------------------------------
-- COMMAND LINE PARSING
--------------------------------------------------------------------

data Options 
    = Options Command

data Command 
    = Read Query Bool
    | List Bool

withInfo :: Opt.Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

pRead :: Opt.Parser Command
pRead = Read
    <$> argument str ( metavar "QUERY" )
    <*> switch ( long "clip" <> short 'c' <> help "Whether to copy results" )

pList :: Opt.Parser Command
pList = List
    <$> switch ( long "show-codes" <> short 's' <> help "List entries with oath codes" )

mySubParsers :: Opt.Parser Command
mySubParsers = subparser $ 
    command "read" (pRead `withInfo` "Read your oath entries") <>
    command "list" (pList `withInfo` "List your oath entries")

options :: Opt.Parser Options
options = Options 
    <$> mySubParsers

--------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (options <**> helper) ( fullDesc <> progDesc "Read and edit your oath entries" <> header "ykham" )

run :: Options -> IO ()
run (Options (Read q True))  = runExceptT (readOath q) >>= handleResult clipPrintCode
run (Options (Read q False)) = runExceptT (readOath q) >>= handleResult printCode 
run (Options (List _))       = runExceptT listOath     >>= handleResult print

-- TODO: result formatting
-- TODO: updating entries
-- TODO: verbosity option?
