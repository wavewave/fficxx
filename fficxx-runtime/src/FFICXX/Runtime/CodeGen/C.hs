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

-- | parts for interpolation
newtype NamePart = NamePart String

newtype CName = CName [NamePart]

sname :: String -> CName
sname s = CName [NamePart s]

renderCName :: CName -> String
renderCName (CName ps) = intercalate "##" $ map (\(NamePart p) -> p) ps


data CDecl =
    FunDecl CType CName [(CType,CName)] -- ^ type func( type1 arg1, type2 arg2, ... )

data CStatement =
    UsingNamespace Namespace -- ^ using namespace <namespace>;
  | TypeDef CType CName      -- ^ typedef origtype newname;
  | CDeclaration CDecl       -- ^ function declaration;
  | CDefinition CDecl [CStatement] -- ^ function definition;
  | CMacroApp CName [CName]  -- ^ C Macro application at statement level (temporary)
  | Comment String           -- ^ comment
  | CEmptyLine               -- ^ for convenience
  | CVerbatim String         -- ^ temporary verbatim

data CMacro =
    CRegular CStatement
  | Include HeaderName       -- ^ #include "<header>"
  | Pragma PragmaParam       -- ^ #pragma
  | Undef CName              -- ^ #undef name
  | Define CName [CName] [CStatement] -- ^ #define macro (type) definition
  | EmptyLine                -- ^ just for convenience
  | Verbatim String          -- ^ temporary verbatim


data CBlock = ExternC [CMacro]

renderPragmaParam :: PragmaParam -> String
renderPragmaParam Once = "once"

renderCDecl :: CDecl -> String
renderCDecl (FunDecl (CType typ) fname args) =
    typ <> " " <> renderCName fname <> " ( " <> intercalate ", " (map mkArgStr args) <> " )"
  where
    mkArgStr (CType t, a) = t <> " " <> renderCName a

-- | render CStatement in a regular environment
renderCStmt :: CStatement -> String
renderCStmt (UsingNamespace (NS ns)) = "using namespace " <> ns <> ";"
renderCStmt (TypeDef (CType typ) n)  = "typedef " <> typ <> " " <> renderCName n <> ";"
renderCStmt (CDeclaration e)         = renderCDecl e <> ";"
renderCStmt (CDefinition d body)     =
  renderCDecl d <> " {\n" <> concatMap renderCStmt body <> "\n}\n"
renderCStmt (CMacroApp n as)         = renderCName n <> "(" <> intercalate ", " (map renderCName as) <> ")" -- NOTE: no semicolon.
renderCStmt (Comment str)            = "// " <> str <> "\n"
renderCStmt CEmptyLine               = "\n"
renderCStmt (CVerbatim str)          = str

-- | render CStatement in a macro definition environment
renderCStmtInMacro :: CStatement -> [String]
renderCStmtInMacro (CDefinition d body) =
     [ renderCDecl d <> " {" ]
  <> map renderCStmt body
  <> [ "}" ]
renderCStmtInMacro (Comment _str)  = [""] -- Comment cannot exist in Macro
renderCStmtInMacro CEmptyLine      = [""]
renderCStmtInMacro (CVerbatim str) = lines str
renderCStmtInMacro s               = [renderCStmt s]

renderCMacro :: CMacro -> String
renderCMacro (CRegular stmt)          = renderCStmt stmt
renderCMacro (Include (HdrName hdr))  = "\n#include \"" <> hdr <> "\"\n"
renderCMacro (Pragma param)           = "\n#pragma " <> renderPragmaParam param <> "\n"
renderCMacro (Undef n)                = "\n#undef " <> renderCName n <> "\n"
renderCMacro (Define m ts stmts)       =
     "\n#define " <> renderCName m
   <> case ts of
        [] -> " "
        _  -> "("  <> intercalate ", " (map renderCName ts) <> ") \\\n"
   <> intercalate "\\\n" (concatMap renderCStmtInMacro stmts)
   <> "\n"
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
