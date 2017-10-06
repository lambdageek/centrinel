-- | Find used of naked pointers to managed regions in C declarations
module Centrinel.NakedPointer.InDeclarations (
  NakedPointerSummary (..)
  , nakedPtrCheckDecl
  ) where

import Control.Monad.Reader.Class
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer.Class
import Control.Monad.Writer (execWriterT)
import Data.Foldable (forM_)

import ZeptoLens

import qualified Language.C.Analysis.SemRep as C
import qualified Language.C.Data.Node as C

import Centrinel.Control.Monad.Class.RegionResult

import Centrinel.NakedPointer.Env
import Centrinel.NakedPointer.Utils
import Centrinel.NakedPointerError (NPEPosn(..)
                                   , NPEVictims
                                   , NakedPointerError
                                   , mkNakedPointerError
                                   )

-- | Given a structure @a@, @nakedPointers a@ is a computation that
-- 'tell's the naked pointers that appear in a.
class NakedPointerSummary a where
  nakedPointers :: (RegionResultMonad m, MonadReader AnalysisEnv m, MonadWriter NPEVictims m) => a -> m ()

instance NakedPointerSummary C.Type where
  nakedPointers ty_ =
    case ty_ of
      C.DirectType {} -> return ()
      C.PtrType ty _qs _attrs -> nakedPointers ty
      C.ArrayType ty _sz _qs _attrs -> nakedPointers ty
      C.TypeDefType tdr _qs _attrs -> nakedPointers tdr
      C.FunctionType fty _attrs -> nakedPointers fty

instance NakedPointerSummary C.TypeDefRef where
  nakedPointers tdr =
    case tdr of
      -- always lookup the typedef even if the type is available - it makes the
      -- victim look nicer.  also if we start avoiding revisiting the same
      -- typedef, this will save us a type traversal.
      --
      -- C.TypeDefRef _ (Just ty) ni ->
      --  local (NPETypeDefRef ni) $ nakedPointers ty
      C.TypeDefRef ident _ {-Nothing-} ni ->
        local (analysisPosn %~ NPETypeDefRef ni) (rrLookupTypedef ident >>= nakedPointers)

instance NakedPointerSummary C.TypeDef where
  nakedPointers td =
    case td of
      C.TypeDef _ident ty _attrs ni ->
        local (analysisPosn %~ NPETypeDefDef ni) (nakedPointers ty)

instance NakedPointerSummary C.FunType where
  nakedPointers fty =
    -- for every type comprising a function type, we do two things:
    -- 1. if it's a pointer, make sure it doesn't point into the managed region;
    -- 2. if it's a function type, recursively check that that function doesn't
    -- have args/return types that point into the managed region.
    --
    -- The former is done by pointerRegionScheme & declPtrRegion,
    -- the latter by recursively calling go.
    --
    -- TODO: this does mean we potentially revisit the same typedef many times.
    -- Maybe use a visited set?
    case fty of
      C.FunTypeIncomplete {} ->
        -- analysis is after typechecking, all params seen already.
        --
        -- FIXME: There's one place where these can still happen.  If a
        -- function is declared without a definition as "int foo ();" then
        -- GlobalDecls still has it as a FunTypeIncomplete
        --
        -- (as expected, "int foo (void);" will be a FunType with no arguments)
        error "unexpected FunTypeIncomplete in nakedPtrCheckIdentDecl"
      C.FunType retty params _variadic -> do
          local (analysisPosn %~ NPERet) $ do
            tellWhenManaged retty
            -- if return is a function, check its args too
            nakedPointers retty
          forM_ (zip params [0..]) $ \(param, j) -> do
            let
              posn :: NPEPosn -> NPEPosn
              posn = NPEArg j (C.declName param) (C.nodeInfo param)
            local (analysisPosn %~ posn) $ do
              let ty = C.declType param
              -- first check if the arg is a ptr to managed
              tellWhenManaged ty
               -- then if arg is a function type, check its args
              nakedPointers ty

nakedPtrCheckDecl :: (RegionResultMonad m, C.Declaration d, C.CNode d) => d -> m (Maybe NakedPointerError)
nakedPtrCheckDecl dcl = do
  let
    initialEnv :: AnalysisEnv
    initialEnv = AnalysisEnv (NPEDecl $ C.declName dcl) False
  npes <- execWriterT $ flip runReaderT initialEnv $ nakedPointers (C.declType dcl)
  return $ case npes of
    [] -> Nothing
    _ ->  Just $ mkNakedPointerError (C.nodeInfo dcl) npes
