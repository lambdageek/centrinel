{-# language GeneralizedNewtypeDeriving #-}
module HeapGuard where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)

import Language.C (parseCFile)
import Language.C.Parser (ParseError)

import Language.C.Syntax.AST (CTranslUnit)

import Language.C.System.GCC (newGCC)

import Language.C.Data.Error (CError)

import qualified Language.C.Analysis.AstAnalysis as A
import qualified Language.C.Analysis.SemRep as A
import qualified Language.C.Analysis.TravMonad as AM

import qualified Language.C.Pretty as P

import qualified HeapGuard.Trav as HG
import qualified HeapGuard.RegionUnification as HG

inp :: FilePath -> IO (Either ParseError CTranslUnit)
inp fp = parseCFile (newGCC "gcc") Nothing [] fp

p :: CTranslUnit -> IO ()
p = print . P.prettyUsingInclude 

newtype HGTrav s a = HGTrav { unHGTrav :: HG.UnifyRegT (AM.Trav s) a}
  deriving (Functor, Applicative, Monad)

instance AM.MonadName (HGTrav s) where
  genName = HGTrav (lift AM.genName)

instance AM.MonadSymtab (HGTrav s) where
  getDefTable = HGTrav (lift AM.getDefTable)
  withDefTable = HGTrav . lift . AM.withDefTable

instance AM.MonadCError (HGTrav s) where
  throwTravError = HGTrav . lift . AM.throwTravError
  catchTravError (HGTrav c) handler = HGTrav (HG.liftCatch AM.catchTravError c (unHGTrav . handler))
  recordError = HGTrav . lift . AM.recordError
  getErrors = HGTrav $ lift AM.getErrors

instance HG.RegionUnification HG.RegionVar (HGTrav s) where
  newRegion = HGTrav HG.newRegion
  sameRegion v1 v2 = HGTrav (HG.sameRegion v1 v2)

instance AM.MonadTrav (HGTrav s) where
  handleDecl = HG.extraDecl

runHGTrav :: HGTrav () a -> Either [CError] (a, [CError])
runHGTrav (HGTrav comp) = AM.runTrav_ (HG.runUnifyRegT comp)

think :: CTranslUnit -> Either [CError] (A.GlobalDecls, [CError])
think u = runHGTrav (A.analyseAST u)
  

think' :: CTranslUnit -> IO A.GlobalDecls
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
