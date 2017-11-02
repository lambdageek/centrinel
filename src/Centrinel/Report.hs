{-# language DeriveGeneric, DeriveFunctor, DefaultSignatures,
      GeneralizedNewtypeDeriving, OverloadedStrings,
      RankNTypes, ScopedTypeVariables, UndecidableInstances
  #-}
module Centrinel.Report (
  -- * Output methods
  OutputMethod(..), OutputFormat(..), OutputDestination(..)
  , defaultOutputMethod
  -- * Output-able computations
  , MonadOutputMethod(..)
  -- * Running output-able computations
  , withOutputMethod
  -- * Reporting
  , presentReport, report
  ) where

import qualified System.IO as IO
import Control.Monad (unless, when)
import Control.Exception (bracket_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import qualified System.Directory as Dir

import Centrinel.Types
import Centrinel.Report.Types
import qualified Centrinel.Report.Json as J

-- | Describes the methods of presenting the analysis results
data OutputMethod = OutputMethod
  { outputMethodDestination :: OutputDestination -- ^ Write results here
  , outputMethodFormat :: OutputFormat -- ^ Write the results in this format
  }

-- | How to present the output
data OutputFormat =
  -- | Present the output in plain text
  PlainTextOutputFormat
  -- | Present the output as a JSON blob
  | JSONOutputFormat

-- | Where to send the output
data OutputDestination =
  -- | Send output to 'IO.stdout'
  StdOutOutputDestination
  -- | Send output to a file with the given 'FilePath'
  | FilePathOutputDestination FilePath

-- | Default output method is plain text on stdOut
defaultOutputMethod :: OutputMethod
defaultOutputMethod = OutputMethod StdOutOutputDestination PlainTextOutputFormat

report :: OutputMethod -> ExceptT CentrinelError IO (a, [CentrinelAnalysisError]) -> IO (Maybe a)
report output comp = do
  x <- runExceptT comp
  withOutputMethod output $ presentReport x

-- | This monad provides operations for working with the reporting monad.
class Monad m => MonadOutputMethod m where
  -- | Emit a message to the report
  present :: Message -> m ()
  -- | Change to the given directory, run the given computation and then
  -- restore the working directory.
  -- The default version of this method uses 'Dir.withWorkingDirectory' provided the monad is an instance of @'MonadBaseControl' IO m@
  withWorkingDirectory :: FilePath -> m a -> m a
  default withWorkingDirectory :: MonadBaseControl IO m => FilePath -> m a -> m a
  withWorkingDirectory fp comp = liftBaseOp_ (Dir.withCurrentDirectory fp) comp
  {-# minimal present #-}

-- | Given either a successful @(x, warns)@ or failing @errors@, @presentReport
-- result@ will 'present' the errors or warnings and return @Just x@ or @Nothing@.
presentReport :: MonadOutputMethod m => Either CentrinelError (a, [CentrinelAnalysisError]) -> m (Maybe a)
presentReport x =
  case x of
    Left centErr -> do
      present (Abnormal centErr)
      return Nothing
    Right (a, warns) -> do
      present (Normal warns)
      return (Just a)

type OutputFn = Message -> IO ()

-- | A monad transformer that adds a 'MonadOutputMethod' to a monad stack.
newtype OutputMethodT m a = OutputMethodT { unOutputMethodT :: ReaderT OutputFn m a }
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (OutputMethodT m) where
  liftIO m = OutputMethodT (liftIO m)

instance (MonadBaseControl IO m, MonadIO m) => MonadOutputMethod (OutputMethodT m) where
  present msg = OutputMethodT (ask >>= \f -> liftIO (f msg))
  withWorkingDirectory fp comp = OutputMethodT $ liftBaseOp_ (Dir.withCurrentDirectory fp) (unOutputMethodT comp)

instance MonadTrans OutputMethodT where
  lift = OutputMethodT . lift

runOutputMethodT :: OutputFn -> OutputMethodT IO a -> IO a
runOutputMethodT outputFn m = runReaderT (unOutputMethodT m) outputFn

-- | Runs the given polymorphic 'MonadOutputMethod' computation in
-- an output monad that sends output via the given 'OutputMethod'
withOutputMethod :: forall a . OutputMethod -> (forall m . (MonadIO m, MonadOutputMethod m) => m a) -> IO a
withOutputMethod (OutputMethod dest fmt) =
  let (header, footer, pres) = case fmt of
        PlainTextOutputFormat -> (const (return ()), const (return ()), plainTextOutput)
        JSONOutputFormat -> (J.header, J.footer, J.output)
      brak h = bracket_ (header h) (footer h)
  in case dest of
    StdOutOutputDestination -> \kont -> brak IO.stdout (runOutputMethodT (pres False IO.stdout) kont)
    FilePathOutputDestination fp -> \kont -> IO.withFile fp IO.AppendMode $ \h ->
      brak h (runOutputMethodT (pres True h) kont)

plainTextOutput :: Bool -> IO.Handle -> Message -> IO ()
plainTextOutput isFile h = \msg ->
  case msg of
    Normal warns ->
      unless (null warns) $ do
      mapM_ (IO.hPutStrLn h . show) warns
      when isFile (summarize "notices" $ length warns)
    Abnormal centErr -> 
      case centErr of
        CentCPPError exitCode ->
          IO.hPutStrLn h $ "Preprocessor failed with " <> show exitCode
        CentParseError err -> IO.hPutStrLn h (show err)
        CentAbortedAnalysisError errs -> do
          mapM_ (IO.hPutStrLn h . show) errs
          when isFile (summarize "notices" $ length errs)
          IO.hPutStrLn h $ "Analysis of translation unit was aborted after the preceeding errors"
  where
    summarize sortOfThings n =
      when (n >= 20) $ IO.hPutStrLn h $ "There were " <> show n <> " " <> sortOfThings

