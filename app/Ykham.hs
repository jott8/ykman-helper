module Main where

import System.Process         ( readProcessWithExitCode )
import System.Environment     ( getArgs )
import System.Hclip           ( setClipboard )

import Control.Monad.Except   ( runExceptT, MonadError(throwError), ExceptT )
import Control.Monad.IO.Class ( liftIO )
import GHC.IO.Exception       ( ExitCode (ExitSuccess) )

import Options.Applicative
    ( (<**>),
      argument,
      command,
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      str,
      subparser,
      switch,
      execParser,
      helper,
      ParserInfo )
import qualified Options.Applicative as Opt

import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List     ( sort )
import Data.Function ( on )

import Fmt ( (+|), fmt, (|+), blockMapF', nameF, Buildable(build) )

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

type Entries  = Map Int Entry

data Entry 
    = MinE Username
    | FullE Issuer Username
    deriving ( Eq, Show )

instance Ord Entry where
    (MinE u)     `compare` (FullE i _)  = (compare `on` T.length) u i
    (FullE i1 _) `compare` (FullE i2 _) = (compare `on` T.length) i1 i2

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
    result@(_,s,e) <- liftIO $ readProcessWithExitCode "ykman" args' stdin'
    if isSuccess result 
        then pure $ T.pack s
        else throwError $ T.pack e

    where
        isSuccess (ExitSuccess,s,_) = not $ null s
        isSuccess _                 = False

        args' = map T.unpack args
        stdin' = T.unpack stdin

readOath :: Query -> YkmanM Code
readOath q = head . T.lines <$> ykman ["oath", "accounts", "code", "-s", q] ""  
          -- ^^^^ safe usage of ´head´ because of ´null´ check in ´isSuccess´

listOath :: YkmanM Entries
listOath = M.fromList <$> indexedEntries
    where
        indexedEntries = index <$> entries
        entries        = map createEntry <$> textEntries
        textEntries    = T.lines <$> ykman ["oath", "accounts", "list"] ""

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

printList :: Entries -> IO ()
printList entries = 
    if M.size entries <= 0
        then TIO.putStrLn "No entries yet..."
        else TIO.putStrLn . fmt . nameF "Your oath entries:" . blockMapF' buildKey buildVal $ entries
    where
        buildKey             = build
        buildVal (MinE u)    = build u 
        buildVal (FullE i u) = ""+|T.justifyLeft longest ' ' i|+" - "+|u|+""

        longest              = getLen $ maximum entries
        getLen (MinE u)      = T.length u
        getLen (FullE i _)   = T.length i

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
run (Options (List _))       = runExceptT listOath     >>= handleResult printList

-- TODO: result formatting
-- TODO: updating entries
-- TODO: verbosity option?
