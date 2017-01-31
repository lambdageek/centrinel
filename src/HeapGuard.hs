module HeapGuard where

import Control.Monad (unless)

import qualified Data.Map as Map

import Language.C (parseCFile)
import Language.C.Parser (ParseError)

import Language.C.Syntax.AST (CTranslUnit)

import Language.C.System.GCC (newGCC)

import Language.C.Data.Ident (SUERef)
import Language.C.Data.Error (CError)

import qualified Language.C.Analysis.AstAnalysis as A
import qualified Language.C.Analysis.SemRep as A

import qualified Language.C.Pretty as P

import qualified HeapGuard.Trav as HG
import HeapGuard.Region (RegionScheme)
import qualified HeapGuard.RegionInference as HG

inp :: FilePath -> IO (Either ParseError CTranslUnit)
inp fp = parseCFile (newGCC "gcc") Nothing [] fp

p :: CTranslUnit -> IO ()
p = print . P.prettyUsingInclude 

type RegionInferenceResult = Map.Map SUERef RegionScheme

think :: CTranslUnit -> -- Either [CError] ((A.GlobalDecls, HG.RegionIdentMap), [CError])
  Either [CError] ((A.GlobalDecls, RegionInferenceResult), [CError])
think u = HG.evalHGTrav HG.inferDeclEvent $ do
  g <- A.analyseAST u
  let tagged = A.gTags g
  regions <- traverse HG.applyBindingTagDef tagged
  return (g, regions)

think' :: CTranslUnit -> IO (A.GlobalDecls, RegionInferenceResult)
think' u =
  case think u of
    Left errs -> do
      putStrLn "Errors:"
      print errs
      return $ error "no global decls"
    Right (g, warns) -> do
      unless (null warns) $ do
        putStrLn "Warnings:"
        print warns
      return g
      
-- example:
-- Right ast <- inp "c-examples/attrib.hs"
-- g <- think' ast
-- P.pretty g
