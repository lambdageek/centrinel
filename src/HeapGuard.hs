module HeapGuard where

import Control.Monad (unless)
import qualified Data.Map as M
import Data.Monoid (First(..))

import Language.C (parseCFile)
import Language.C.Parser (ParseError)

import Language.C.Syntax.AST (CTranslUnit)
import qualified Language.C.Syntax.AST as Syn
import qualified Language.C.Syntax.Constants as Syn

import Language.C.System.GCC (newGCC)

import Language.C.Data.Error (CError)
import qualified Language.C.Data.Error as Err

import Language.C.Data.Node (NodeInfo)
import qualified Language.C.Data.Node as N
import qualified Language.C.Data.Ident as Id

import qualified Language.C.Analysis.AstAnalysis as A
import qualified Language.C.Analysis.SemRep as A
import qualified Language.C.Analysis.TravMonad as AM

import qualified Language.C.Pretty as P

import qualified Language.C.Analysis.Debug as D

inp :: FilePath -> IO (Either ParseError CTranslUnit)
inp fp = parseCFile (newGCC "gcc") Nothing [] fp

p :: CTranslUnit -> IO ()
p = print . P.prettyUsingInclude 

newtype LexWarn = LexWarn Err.ErrorInfo

instance Err.Error LexWarn where
  errorInfo (LexWarn e) = e
instance Show LexWarn where
  showsPrec p (LexWarn e) = showsPrec p e

lexWarn :: String -> NodeInfo -> LexWarn
lexWarn msg ni = LexWarn $ Err.mkErrorInfo Err.LevelWarn msg ni

think :: CTranslUnit -> Either [CError] (A.GlobalDecls, [CError])
think u = AM.runTrav_ $ AM.withExtDeclHandler (A.analyseAST u) extraDecl
  where
    extraDecl e = case e of
      A.TagEvent (A.CompDef structTy@(A.CompType _ A.StructTag (_:_) _ _)) -> thinkStruct structTy
      _ -> return ()

newtype Region = Region Int
  deriving (Show, Eq)

hasRegionAttr :: A.Attributes -> Maybe Region
hasRegionAttr = getFirst . foldMap (First . from)
  where
    from (A.Attr ident [Syn.CConst (Syn.CIntConst r _)] _ni) | Id.identToString ident == "__region" = Just (Region $ fromInteger $ Syn.getCInteger r)
    from _ = Nothing
  

thinkStruct :: AM.MonadTrav m => A.CompType -> m ()
thinkStruct (A.CompType suref A.StructTag (firstMember:_) attrs ni) =
  case hasRegionAttr attrs of
    Just r ->
      AM.warn (lexWarn ("Found struct " ++  show (P.pretty suref) ++ " with at least one member and region " ++ show r) ni)
    Nothing -> return ()
thinkStruct _ = fail "can't happen"

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
