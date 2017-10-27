-- | Convenience functions for exploring Centrinel in a GHCi
module Centrinel.InRepl where

import Control.Monad.Except

import Language.C.Syntax.AST (CTranslUnit)
import qualified Language.C.System.Preprocess as CPP
import Language.C.System.GCC (newGCC)
import qualified Language.C.Analysis.SemRep as A

import Centrinel
import Centrinel.Types
import Centrinel.Report
import Centrinel.RegionInferenceResult (RegionInferenceResult)
import qualified Centrinel.PrettyPrint as P
import qualified Centrinel.NakedPointer as NP

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
think' npOpts fp = report defaultOutputMethod (inp >>= think npOpts)
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

