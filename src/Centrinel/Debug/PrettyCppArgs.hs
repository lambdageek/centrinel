{-# language NamedFieldPuns #-}
-- | Pretty print 'Language.C.System.Preprocess.CppArgs'
module Centrinel.Debug.PrettyCppArgs
  ( showCppArgs
  , prettyCppArgs
  ) where

import Language.C.System.Preprocess (CppArgs(..), CppOption (..))

import qualified Centrinel.PrettyPrint as PP

showCppArgs :: CppArgs -> String
showCppArgs = PP.render . prettyCppArgs

-- | 
prettyCppArgs :: CppArgs -> PP.Doc
prettyCppArgs (CppArgs { cppOptions, extraOptions, cppTmpDir, inputFile, outputFile }) =
  ppC "CppArgs"
  $ PP.braces $ PP.fsep
  $ PP.punctuate PP.comma
  $ map (PP.nest 4 . ppEqualities) [ ("cppOptions", prettyOptions)
                                   , ("extraOptions", prettyExtraOptions)
                                   , ("cppTmpDir", prettyTmpDir)
                                   , ("inputFile", prettyInputFile)
                                   , ("outputFile", prettyOutputFile)
                                   ]
  where
    ppEqualities (a,b) = PP.fsep [PP.text a, PP.text "=", PP.nest 4 b]

    prettyOptions = ppList $ map prettyCppOption cppOptions
    prettyExtraOptions = ppList $ map ppQuoted extraOptions
    prettyTmpDir = ppMaybe ppQuoted cppTmpDir
    prettyInputFile = ppQuoted inputFile
    prettyOutputFile = ppMaybe ppQuoted outputFile

    ppMaybe f = maybe (PP.text "Nothing") (ppC "Just" . f)
    ppList = PP.brackets . PP.sep . PP.punctuate PP.comma . map (PP.nest 2)

    ppQuoted = PP.doubleQuotes . PP.text

    ppC c d = PP.hang (PP.text c) 2 d

    prettyCppOption :: CppOption -> PP.Doc
    prettyCppOption opt =
      case opt of
        IncludeDir fp   -> ppC "IncludeDir"  $ ppQuoted fp
        Define name val -> ppC "Define"      $ PP.sep [ppQuoted name, ppQuoted val]
        Undefine name   -> ppC "Undefine"    $ ppQuoted name
        IncludeFile fp  -> ppC "IncludeFile" $ ppQuoted fp
