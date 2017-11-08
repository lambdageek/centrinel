module Centrinel.NakedPointerError where

import Data.Monoid ((<>))

import qualified Language.C.Data.Node as C
import qualified Language.C.Analysis.SemRep as C
import qualified Language.C.Data.Error as Err
    
import qualified Centrinel.PrettyPrint as PP
import Centrinel.PrettyPrint ((<+>))

import Language.C.Analysis.Debug () -- instance PP.Pretty Type

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

instance PP.Pretty NPEPosn where
  pretty p0 = PP.vcat $ case p0 of
    NPEDecl ident -> [PP.text "in function declaration" <+> PP.quotes (PP.pretty ident)]
    NPEArg j ident ni p -> [PP.text "in" <+> prettyOrdinal j <+> PP.text "argument" <+> PP.quotes (PP.pretty ident)
                             <+> PP.text "at" <+> PP.prettyPos ni, PP.pretty p]
    NPERet p -> [PP.text "in return type", PP.pretty p]
    NPETypeDefRef ni p -> [PP.text "at" <+> PP.prettyPos ni, PP.pretty p]
    NPETypeDefDef ni p -> [PP.text "in type defined at" <+> PP.prettyPos ni, PP.pretty p]
    NPEDefn ident -> [PP.text "in the definition of " <+> PP.quotes (PP.pretty ident)]
    NPEStmt ni p -> [PP.text "in statement at " <+> PP.prettyPos ni, PP.pretty p]
    NPETypeOfExpr ni p -> [PP.text "in the type of the expression at " <+> PP.prettyPos ni, PP.pretty p]

prettyOrdinal :: Integral n => n -> PP.Doc
prettyOrdinal n = PP.integer (toInteger n) PP.<> suf
  where
    r = n `mod` 10
    suf = case r of
            1 | h /= 11 -> PP.text "st"
            2 | h /= 12 -> PP.text "nd"
            3 | h /= 13 -> PP.text "rd"
            _ -> PP.text "th"
    h = n `mod` 100

-- | Trace of an error position
data NPEPosn = NPEArg !Int !C.VarName !C.NodeInfo !NPEPosn -- ^ function argument j
  | NPERet !NPEPosn -- ^ function return value
  | NPEDecl !C.VarName -- ^ a declaration
  | NPETypeDefRef !C.NodeInfo !NPEPosn -- ^ a typedef occurrence
  | NPETypeDefDef !C.NodeInfo !NPEPosn -- ^ the typedef declaration
  | NPEDefn !C.VarName -- ^ a (function) definition
  | NPEStmt !C.NodeInfo !NPEPosn -- ^ a statement in a function
  | NPETypeOfExpr !C.NodeInfo  !NPEPosn -- ^ in the type of the expression

mkNakedPointerError :: Bool -> C.NodeInfo -> [NPEVictim] -> NakedPointerError
mkNakedPointerError inDefn ni npes = NakedPointerError inDefn ni npes Err.LevelWarn

instance Err.Error NakedPointerError where
  errorInfo (NakedPointerError inDefn ni victims lvl) = Err.mkErrorInfo lvl msg ni
    where
      msg = PP.render $ PP.vcat (msghead : map PP.pretty victims)
      msghead = PP.text ("Naked pointer(s) to managed object(s) found in "
                         <> if inDefn then "definition" else "declaration")
  changeErrorLevel npe lvl =
    npe { npeErrLvl = lvl }
