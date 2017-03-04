module Centrinel where

import Control.Monad (unless, when, liftM)

import Control.Monad.Except (ExceptT(..), withExceptT, runExceptT)

import qualified System.Exit

import Language.C.Parser (ParseError, parseC)

import Language.C.Syntax.AST (CTranslUnit)

import Language.C.Data.Error (CError, changeErrorLevel, ErrorLevel(LevelWarn))
import Language.C.Data.Position (initPos)

import Language.C.System.GCC (newGCC, GCC)
import qualified Language.C.System.Preprocess as CPP

import qualified Language.C.Analysis.AstAnalysis as A
import qualified Language.C.Analysis.SemRep as A
import qualified Language.C.Analysis.TravMonad as A

import qualified Centrinel.PrettyPrint as P

import qualified Centrinel.Trav as HG
import qualified Centrinel.RegionInference as HG
import Centrinel.RegionInferenceResult

import qualified Centrinel.NakedPointer as NP
import qualified Centrinel.RegionResultMonad as NP

import qualified Centrinel.Util.Datafiles as HGData

makeNakedPointerOpts :: FilePath -> NP.AnalysisOpts
makeNakedPointerOpts fp = NP.AnalysisOpts {NP.analysisOptFilterPath = Just fp }

data CentrinelError =
  CentCPPError !System.Exit.ExitCode
  | CentParseError !ParseError
  | CentAnalysisError ![CentrinelAnalysisError]

type CentrinelAnalysisError = CError


-- | Remove any 'CPP.outputFile' options, and add
-- preprocessor defines and definitions for Centrinel to successfully parse and
-- analize the specified input file.
cppArgsForCentrinel :: CPP.CppArgs -> HGData.Datafiles -> CPP.CppArgs
cppArgsForCentrinel cppArgs datafiles =
  let centrinelHeader = HGData.datafileCentrinelHeader datafiles
  in cppArgs
     { CPP.cppOptions = CPP.cppOptions cppArgs ++ [ CPP.IncludeFile centrinelHeader ]
     , CPP.outputFile = Nothing
     }

parseCFile :: CPP.Preprocessor cpp => cpp -> CPP.CppArgs -> ExceptT CentrinelError IO CTranslUnit
{-# specialize parseCFile :: GCC -> CPP.CppArgs -> ExceptT CentrinelError IO CTranslUnit #-}
parseCFile cpp cppArgs = do
    inputStream <- withExceptT CentCPPError $ ExceptT $ CPP.runPreprocessor cpp cppArgs
    withExceptT CentParseError $ ExceptT $ return $ parseC inputStream (initPos $ CPP.inputFile cppArgs)

p :: CTranslUnit -> IO ()
p = print . P.prettyUsingInclude 

pp :: P.Pretty a => a -> IO ()
pp = print . P.pretty

getInferredRegions :: A.GlobalDecls -> HG.HGTrav s RegionInferenceResult
getInferredRegions g = do
  let structDefs = HG.justStructTagDefs (A.gTags g)
  makeRegionInferenceResult <$> traverse HG.applyBindingTagDef structDefs

inferRegions :: CTranslUnit -> HG.HGTrav s (A.GlobalDecls, RegionInferenceResult)
inferRegions u = do
  g <- HG.withHGAnalysis (nonFatal . HG.inferDeclEvent) $ A.analyseAST u
  regions <- getInferredRegions g
  return (g, regions)
  where
    -- catch any errors due to this declaration, record them and continue.
    nonFatal :: A.MonadCError m => m () -> m ()
    nonFatal comp = A.catchTravError comp (\e -> A.recordError $ changeErrorLevel e LevelWarn)

think :: Monad m => NP.AnalysisOpts -> CTranslUnit -> ExceptT CentrinelError m ((A.GlobalDecls, RegionInferenceResult), [CError])
think npOpts u = withExceptT CentAnalysisError $ HG.evalHGTrav $ do
  grir@(g,rir) <- inferRegions u
  NP.runInferenceResultT (NP.analyze npOpts $ A.gObjs g) (A.gTypeDefs g) rir
  return grir

report :: Show w => ExceptT CentrinelError IO (a, [w]) -> IO System.Exit.ExitCode
report comp = do
  x <- runExceptT comp
  case x of
    Left centErr -> do
      case centErr of
        CentCPPError exitCode -> print $ "Preprocessor failed with " ++ show exitCode
        CentParseError err -> print err
        CentAnalysisError errs -> do
          putStrLn "Errors:"
          mapM_ print errs
          let n = length errs
          when (n > 20) $ putStrLn ("There were " ++ show n ++ " errors")
      return (System.Exit.ExitFailure 1)
    Right (_, warns) -> do
      unless (null warns) $ do
        putStrLn "Warnings:"
        mapM_ print warns
      return System.Exit.ExitSuccess
    
-- | Run the preprocessor with the given arguments, parse the result and run
-- the Centrinel analysis.
runCentrinel :: CPP.Preprocessor cpp => HGData.Datafiles -> cpp -> CPP.CppArgs -> ExceptT CentrinelError IO ((), [CentrinelAnalysisError])
{-# specialize runCentrinel :: HGData.Datafiles -> GCC -> CPP.CppArgs -> ExceptT CentrinelError IO ((), [CentrinelAnalysisError]) #-}
runCentrinel datafiles cpp cppArgs_ = do
  let cppArgs = cppArgsForCentrinel cppArgs_ datafiles
  ast <- parseCFile cpp cppArgs
  let opts = makeNakedPointerOpts (CPP.inputFile cppArgs)
  liftM (\(_, warns) -> ((), warns)) (think opts ast)

-- | Don't use this for real, just in ghci
-- example:
-- >>> let fp = "c-examples/attrib.hs"
-- >>> let opts = makeNakedPointerOpts fp
-- >>> think' opts fp
think' :: NP.AnalysisOpts -> FilePath -> IO System.Exit.ExitCode
think' npOpts fp = report (inp >>= think npOpts)
  where
    inp :: ExceptT CentrinelError IO CTranslUnit
    inp = parseCFile (newGCC "cc") cpp_args

    cpp_args = (CPP.rawCppArgs preprocessorCmdLine fp) { CPP.cppTmpDir = Nothing }

    -- blatantly stolen from an autoconf run for playing around with ghci
    preprocessorCmdLine :: [String]
    preprocessorCmdLine = [ "-DHAVE_CONFIG_H"
         , "-I."
         , "-I../.."
         , "-I../.."
         , "-I../../mono"
         , "-I../../libgc/include"
         , "-I../../eglib/src"
         , "-I../../eglib/src"
         , "-D_THREAD_SAFE"
         , "-DGC_MACOSX_THREADS"
         , "-DPLATFORM_MACOSX"
         , "-DUSE_MMAP"
         , "-DUSE_MUNMAP"
         , "-DMONO_DLL_EXPORT"
         ]

