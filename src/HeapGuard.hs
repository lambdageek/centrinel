module HeapGuard where

import Control.Monad (unless, when)

import Language.C (parseCFile)
import Language.C.Parser (ParseError)

import Language.C.Syntax.AST (CTranslUnit)

import Language.C.System.GCC (newGCC)

import Language.C.Data.Error (changeErrorLevel, ErrorLevel(LevelWarn))

import qualified Language.C.Analysis.AstAnalysis as A
import qualified Language.C.Analysis.SemRep as A
import qualified Language.C.Analysis.TravMonad as A

import qualified HeapGuard.PrettyPrint as P

import qualified HeapGuard.Trav as HG
import qualified HeapGuard.RegionInference as HG
import HeapGuard.RegionInferenceResult

import qualified HeapGuard.NakedPointer as NP
import qualified HeapGuard.RegionResultMonad as NP



makeNakedPointerOpts :: FilePath -> NP.AnalysisOpts
makeNakedPointerOpts fp = NP.AnalysisOpts {NP.analysisOptFilterPath = Just fp }

inp :: FilePath -> IO (Either ParseError CTranslUnit)
inp fp = parseCFile (newGCC "cc") Nothing preprocessorCmdLine fp
  where
    preprocessorCmdLine :: [String]
    preprocessorCmdLine = ["-D__HEAPGUARD__", "-U__BLOCKS__"]
      ++ [ "-DHAVE_CONFIG_H"
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

think' :: NP.AnalysisOpts -> CTranslUnit -> IO (A.GlobalDecls, RegionInferenceResult)
think' npOpts u = do
  let work = do
        grir@(g,rir) <- inferRegions u
        NP.runInferenceResultT (NP.analyze npOpts $ A.gObjs g) (A.gTypeDefs g) rir
        return grir
  case HG.evalHGTrav work of
    Left errs -> do
      putStrLn "Errors:"
      mapM_ print errs
      let n = length errs
      when (n > 20) $ putStrLn ("There were " ++ show n ++ " errors")
      return $ error "no global decls"
    Right (grir, warns) -> do
      unless (null warns) $ do
        putStrLn "Warnings:"
        mapM_ print warns
      return grir
    
      
-- example:
-- Right ast <- inp "c-examples/attrib.hs"
-- g <- think' ast
-- P.pretty g
