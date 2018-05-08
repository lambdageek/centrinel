module Centrinel.NakedPointerError where

import Data.Monoid ((<>))

import qualified Language.C.Data.Node as C
import qualified Language.C.Analysis.SemRep as C
import qualified Language.C.Data.Error as Err
    
import qualified Centrinel.PrettyPrint as PP
import Centrinel.PrettyPrint ((<+>))

import Language.C.Analysis.Debug () -- instance PP.Pretty Type

import Centrinel.Data.CodePosition

-- | A naked pointer to the managed heap in a function declaration or definiton.
data NakedPointerError =
  NakedPointerError
  { inDefinition :: !Bool -- ^ 'True' if error is in the body of a definition, 'False' if a declaration
  , primaryNode :: !C.NodeInfo -- ^ The definition or declaration in question
  , victims :: !NPEVictims -- ^ The specific positions within the defn or decl that have naked pointers.
  , npeErrLvl :: !Err.ErrorLevel -- ^ An error level in order to implement 'Err.changeErrorLevel'
  }

instance Show NakedPointerError where
  show = Err.showError ""
  
-- | A trace to a specific position (and its type) in a definition or
-- declaration that has a naked pointer to the managed heap.
data NPEVictim = NPEVictim !C.Type !NPEPosn

type NPEVictims = [NPEVictim]

instance PP.Pretty NPEVictim where
  pretty (NPEVictim ty pos) = PP.fsep [PP.text "Pointer to managed heap" <+> PP.pretty ty
                                      , PP.nest 8 (PP.pretty pos)
                                      ]

instance Show NPEVictim where
  show = PP.render . PP.pretty


mkNakedPointerError :: Bool -> C.NodeInfo -> [NPEVictim] -> NakedPointerError
mkNakedPointerError inDefn ni npes = NakedPointerError inDefn ni npes Err.LevelWarn

instance Err.Error NakedPointerError where
  errorInfo (NakedPointerError inDefn ni vs lvl) = Err.mkErrorInfo lvl msg ni
    where
      msg = PP.render $ PP.vcat (msghead : map PP.pretty vs)
      msghead = PP.text ("Naked pointer(s) to managed object(s) found in "
                         <> if inDefn then "definition" else "declaration")
  changeErrorLevel npe lvl =
    npe { npeErrLvl = lvl }
