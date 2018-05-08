-- | Convenience functions for exploring Centrinel in a GHCi
module Centrinel.InRepl where

import Control.Monad.Except

import Language.C.Syntax.AST (CTranslUnit)
import qualified Language.C.System.Preprocess as CPP
import Language.C.System.GCC (GCC, newGCC)
import qualified Language.C.Analysis.SemRep as A

import Centrinel.Types
import Centrinel.AnalysisPlan (defaultPlan, runPlan)
import Centrinel.Report
import Centrinel.RegionInferenceResult (RegionInferenceResult)
import qualified Centrinel.PrettyPrint as P
import qualified Centrinel.NakedPointer as NP
import Centrinel.System.ParseCFile (parseCFile)
import Centrinel.System.RunLikeCC (cppArgsForCentrinel)
import Centrinel.Util.Datafiles as Datafiles

p :: CTranslUnit -> IO ()
p = print . P.prettyUsingInclude 

pp :: P.Pretty a => a -> IO ()
pp = print . P.pretty

-- | Don't use this for real, just in ghci
-- example:
-- >>> let fp = "c-examples/attrib.hs"
-- >>> let opts = makeNakedPointerOpts fp
-- >>> think' opts fp
think' :: NP.AnalysisOpts -> FilePath -> IO (Maybe (A.GlobalDecls, RegionInferenceResult))
think' npOpts fp = report defaultOutputMethod fp (inp >>= runPlan defaultPlan npOpts)
  where
    inp :: ExceptT CentrinelFatalError IO CTranslUnit
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

-- | Run the preprocessor with the given arguments, parse the result and run
-- the Centrinel analysis.
runCentrinel :: CPP.Preprocessor cpp => Datafiles.Datafiles -> cpp -> CPP.CppArgs -> ExceptT CentrinelFatalError IO ((), [CentrinelAnalysisError])
{-# specialize runCentrinel :: Datafiles.Datafiles -> GCC -> CPP.CppArgs -> ExceptT CentrinelFatalError IO ((), [CentrinelAnalysisError]) #-}
runCentrinel datafiles cpp cppArgs_ = do
  let cppArgs = cppArgsForCentrinel cppArgs_ datafiles
  ast <- parseCFile cpp cppArgs
  let opts = makeNakedPointerOpts (CPP.inputFile cppArgs)
  fmap (\(_, warns) -> ((), warns)) (runPlan defaultPlan opts ast)

makeNakedPointerOpts :: FilePath -> NP.AnalysisOpts
makeNakedPointerOpts fp = NP.AnalysisOpts {NP.analysisOptFilterPath = Just fp }
