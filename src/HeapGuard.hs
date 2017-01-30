module HeapGuard where

import Control.Monad (unless)

import Language.C (parseCFile)
import Language.C.Parser (ParseError)

import Language.C.Syntax.AST (CTranslUnit)

import Language.C.System.GCC (newGCC)

import Language.C.Data.Error (CError)

import qualified Language.C.Analysis.AstAnalysis as A
import qualified Language.C.Analysis.SemRep as A

import qualified Language.C.Pretty as P

import qualified HeapGuard.Trav as HG
import qualified HeapGuard.RegionInference as HG

inp :: FilePath -> IO (Either ParseError CTranslUnit)
inp fp = parseCFile (newGCC "gcc") Nothing [] fp

p :: CTranslUnit -> IO ()
p = print . P.prettyUsingInclude 

think :: CTranslUnit -> Either [CError] ((A.GlobalDecls, HG.RegionIdentMap), [CError])
think u = HG.runHGTrav HG.inferDeclEvent (A.analyseAST u)
  

think' :: CTranslUnit -> IO (A.GlobalDecls, HG.RegionIdentMap)
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
