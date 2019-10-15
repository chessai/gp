{-# language
    DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , LambdaCase
  , NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , ScopedTypeVariables
  , TypeApplications
  #-}

module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.DeepSeq (rnf)
import Control.Monad (void)
import Data.Map (Map)
import Data.Monoid (Alt(..))
import Dhall
import GHC.Generics (Generic)
import qualified Control.Exception as E
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.String as Str
import qualified Options.Applicative as O
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Posix.Process as P

-- example: sudo GP_CONF="/path/to/gp/config" su -p -c 'gp --help'
main :: IO ()
main = do
  config <- loadConfig
  withGps config $ \gps -> do
    let pool = constructPool gps
    let parser = gpParsers gps
    let prog = O.info (parser O.<**> O.helper) O.fullDesc
    gp_name <- do
      args <- Env.getArgs
      case O.execParserPure O.defaultPrefs prog args of
        O.Success (Just gp_name) -> do
          pure gp_name
        O.Success Nothing -> do
          progn <- Env.getProgName
          IO.hPutStrLn IO.stderr (progn <> ": not passed a host to connect to. Try " <> progn <> " --help.\n")
          Exit.exitFailure
        O.Failure failure -> do
          progn <- Env.getProgName
          let (msg,exit) = O.renderFailure failure progn
          case exit of
            Exit.ExitSuccess -> putStrLn msg
            _ -> IO.hPutStrLn IO.stderr msg
          Exit.exitWith exit
        O.CompletionInvoked compl -> do
          progn <- Env.getProgName
          msg <- O.execCompletion compl progn
          putStr msg
          Exit.exitSuccess
    withGpName gp_name pool $ \gp -> do
      runGp gp

-- TODO: test that we have openconnect.
-- it must be version >= 8.
--testOpenConnect :: IO ()
--testOpenConnect = do

runGp :: Gp -> IO ()
runGp Gp{..} = do
  putStrLn $ "Attempting to establish a connection to "
          <> host
          <> "\n"
  let args = [ "--protocol=gp", host ] ++ case servercert of
        Nothing -> []
        Just s -> ["--servercert=" <> s]

  void $ forkIO $ case (username,password) of
    (Just u, Just p) -> fillCreds u p
    _ -> pure ()

  P.executeFile "openconnect" True args Nothing

fillUsername :: String -> IO ()
fillUsername username = do
  output <- IO.hGetContents IO.stdout
  withForkWait (E.evaluate $ rnf output) $ \waitOut -> do
    if "Username: " `isSuffixOf` output
      then IO.hPutStr IO.stdin (username ++ "\n")
      else do
        waitOut
        fillUsername username

fillPassword :: String -> IO ()
fillPassword password = do
  output <- IO.hGetContents IO.stdout
  -- do not show password
  oldEchoSetting <- IO.hGetEcho IO.stdin
  let newEchoSetting = False
  withForkWait (E.evaluate $ rnf output) $ \waitOut -> do
    if "Password: " `isSuffixOf` output
      then do
        E.bracket_
          (IO.hSetEcho IO.stdin newEchoSetting)
          (IO.hSetEcho IO.stdin oldEchoSetting)
          (IO.hPutStr IO.stdin (password ++ "\n"))
      else do
        waitOut
        fillPassword password

fillCreds :: String -> String -> IO ()
fillCreds username password = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  fillUsername username
  fillPassword password

isSuffixOf :: Eq a => [a] -> [a] -> Bool
x `isSuffixOf` xs = L.isPrefixOf (reverse x) (reverse xs)

withForkWait :: IO () -> (IO () -> IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar @(Either E.SomeException ())
  E.mask $ \restore -> do
    tid <- forkIO $ E.try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either E.throwIO pure
    restore (body wait) `E.onException` killThread tid

data GpError
  = GpConfEnvNotFound
  | ConfigFileNotFound String
  | NameNotFound String
  deriving stock (Eq, Show)

withGps :: Either GpError [Gp] -> ([Gp] -> IO a) -> IO a
withGps (Left gp_err) _ = do
  putStrLn (show gp_err)
  Exit.exitFailure
withGps (Right gps) f = do
  f gps

withGpName :: String -> Map String Gp -> (Gp -> IO a) -> IO a
withGpName name m f = case Map.lookup name m of
  Nothing -> do
    putStrLn (show (NameNotFound name))
    Exit.exitFailure
  Just gp -> do
    f gp

loadConfig :: IO (Either GpError [Gp])
loadConfig = do
  mconf <- Env.lookupEnv "GP_CONF"
  case mconf of
    Nothing -> do
      pure (Left GpConfEnvNotFound)
    Just conf -> do
      exists <- Dir.doesFileExist conf
      if exists
        then do
          gps <- input auto (pack conf)
          pure (Right gps)
        else do
          pure (Left (ConfigFileNotFound conf))

constructPool :: [Gp] -> Map String Gp
constructPool = foldr
  (\(gp@Gp{..}) m -> Map.insert name gp m
  )
  mempty

isPicked :: String -> Bool -> Maybe String
isPicked name b = if b
  then Just name
  else Nothing

pack :: Str.IsString s => String -> s
pack = Str.fromString

gpParser :: Gp -> O.Parser (Maybe String)
gpParser Gp{..} = fmap (isPicked name) $ O.switch
  ( O.long name
  <> O.help ("establish connection to " <> host)
  )

gpParsers :: [Gp] -> O.Parser (Maybe String)
gpParsers = getAlt . foldMap (Alt . gpParser)

data Gp = Gp
  { name :: String
    -- ^ alias for this host.
  , host :: String
  --, hostIp :: Maybe IPv4
  --  -- ^ ipv4 address of the host
  --, hostName :: Maybe String
  --  -- ^ dns name of host
  , servercert :: Maybe String
    -- ^ sometimes servers want a cert (usually identified by a hash)
  , username :: Maybe String
    -- ^ username
  , password :: Maybe String
    -- ^ password
  }
  deriving stock (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (Interpret)
