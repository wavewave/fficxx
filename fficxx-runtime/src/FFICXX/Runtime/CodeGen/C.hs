{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FFICXX.Runtime.CodeGen.C where

import Data.Hashable  ( Hashable )
import Data.List      ( intercalate )
import Data.Semigroup ( (<>) )
import Data.String    ( IsString(..) )


newtype HeaderName =
  HdrName { unHdrName :: String }
  deriving (Hashable, Show, Eq, Ord)


instance IsString HeaderName where
  fromString = HdrName

newtype Namespace =
  NS { unNamespace :: String }
  deriving (Show,Eq,Ord)

instance IsString Namespace where
  fromString = NS

data PragmaParam = Once

data CType = CType String

-- | interpolation item
data IItem = AsIs String | Var String

newtype CName = CName [IItem]

sname :: String -> CName
sname s = CName [AsIs s]

renderCName :: CName -> String
renderCName (CName items) = concatMap renderIItem items
  where
    renderIItem (AsIs s) = s
    renderIItem (Var v) = "## " <> v <> " ##"


data CDecl =
    FunDecl CType CName [(CType,CName)] -- ^ type func( type1 arg1, type2 arg2, ... )

data CStatement =
    UsingNamespace Namespace -- ^ using namespace <namespace>;
  | TypeDef CType CName       -- ^ typedef origtype newname;
  | CDeclaration CDecl       -- ^ function declaration
  | Comment String           -- ^ comment

data CMacro =
    CRegular CStatement
  | Include HeaderName       -- ^ #include "<header>"
  | Pragma PragmaParam       -- ^ #pragma
  | EmptyLine                -- ^ just for convenience
  | Verbatim String          -- ^ temporary verbatim


data CBlock = ExternC [CMacro]

renderPragmaParam :: PragmaParam -> String
renderPragmaParam Once = "once"

renderCDecl :: CDecl -> String
renderCDecl (FunDecl (CType typ) fname args) =
    typ <> " " <> renderCName fname <> " ( " <> intercalate "," (map mkArgStr args) <> " )"
  where
    mkArgStr (CType t, a) = t <> " " <> renderCName a

renderCStmt :: CStatement -> String
renderCStmt (UsingNamespace (NS ns)) = "using namespace " <> ns <> ";"
renderCStmt (TypeDef (CType typ) n)  = "typedef " <> typ <> " " <> renderCName n <> ";"
renderCStmt (CDeclaration e)         = renderCDecl e <> ";"
renderCStmt (Comment str)            = "// " <> str <> "\n"

renderCMacro :: CMacro -> String
renderCMacro (CRegular stmt)          = renderCStmt stmt
renderCMacro (Include (HdrName hdr))  = "\n#include \"" <> hdr <> "\"\n"
renderCMacro (Pragma param)           = "\n#pragma " <> renderPragmaParam param <> "\n"
renderCMacro EmptyLine                = "\n"
renderCMacro (Verbatim str)           = str


renderBlock :: CBlock -> String
renderBlock (ExternC ms) =
     "\n#ifdef __cplusplus\n\
     \extern \"C\" {\n\
     \#endif\n"
  ++ concatMap renderCMacro ms
  ++ "\n#ifdef __cplusplus\n\
     \}\n\
     \#endif\n"
