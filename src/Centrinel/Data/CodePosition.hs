-- | Descriptive position in C code
module Centrinel.Data.CodePosition where

import qualified Language.C.Data.Node as C
import qualified Language.C.Analysis.SemRep as C
import Language.C.Analysis.Debug () -- instance PP.Pretty Type

import qualified Centrinel.PrettyPrint as PP
import Centrinel.PrettyPrint ((<+>))

-- | Trace of an error position
data NPEPosn = NPEArg !Int !C.VarName !C.NodeInfo !NPEPosn -- ^ function argument j
  | NPERet !NPEPosn -- ^ function return value
  | NPEDecl !C.VarName -- ^ a declaration
  | NPETypeDefRef !C.NodeInfo !NPEPosn -- ^ a typedef occurrence
  | NPETypeDefDef !C.NodeInfo !NPEPosn -- ^ the typedef declaration
  | NPEDefn !C.VarName -- ^ a (function) definition
  | NPEStmt !C.NodeInfo !NPEPosn -- ^ a statement in a function
  | NPETypeOfExpr !C.NodeInfo  !NPEPosn -- ^ in the type of the expression

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
