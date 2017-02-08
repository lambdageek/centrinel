module HeapGuard where

import Control.Monad (unless)

import qualified Data.Assoc

import Language.C (parseCFile)
import Language.C.Parser (ParseError)

import Language.C.Syntax.AST (CTranslUnit)

import Language.C.System.GCC (newGCC)

import Language.C.Data.Ident (SUERef)
import Language.C.Data.Error (changeErrorLevel, ErrorLevel(LevelWarn))

import qualified Language.C.Analysis.AstAnalysis as A
import qualified Language.C.Analysis.SemRep as A
import qualified Language.C.Analysis.TravMonad as A

import qualified Language.C.Pretty as P

import qualified HeapGuard.Trav as HG
import HeapGuard.Region (RegionScheme)
import qualified HeapGuard.RegionInference as HG

inp :: FilePath -> IO (Either ParseError CTranslUnit)
inp fp = parseCFile (newGCC "gcc") Nothing [] fp

p :: CTranslUnit -> IO ()
p = print . P.prettyUsingInclude 

pp :: P.Pretty a => a -> IO ()
pp = print . P.pretty

type RegionInferenceResult = Data.Assoc.Assoc SUERef RegionScheme

getInferredRegions :: A.GlobalDecls -> HG.HGTrav s RegionInferenceResult
getInferredRegions g = do
  let tagged = A.gTags g
  Data.Assoc.Assoc <$> traverse HG.applyBindingTagDef tagged

inferRegions :: CTranslUnit -> HG.HGTrav s (A.GlobalDecls, RegionInferenceResult)
inferRegions u = do
  g <- HG.withHGAnalysis (nonFatal . HG.inferDeclEvent) $ A.analyseAST u
  regions <- getInferredRegions g
  return (g, regions)
  where
    -- catch any errors due to this declaration, record them and continue.
    nonFatal :: A.MonadCError m => m () -> m ()
    nonFatal comp = A.catchTravError comp (\e -> A.recordError $ changeErrorLevel e LevelWarn)

think' :: CTranslUnit -> IO (A.GlobalDecls, RegionInferenceResult)
think' u =
  case HG.evalHGTrav (inferRegions u) of
    Left errs -> do
      putStrLn "Errors:"
      print errs
      return $ error "no global decls"
    Right (grir, warns) -> do
      unless (null warns) $ do
        putStrLn "Warnings:"
        print warns
      return grir
      
-- example:
-- Right ast <- inp "c-examples/attrib.hs"
-- g <- think' ast
-- P.pretty g
