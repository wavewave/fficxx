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

data CType = CTVerbatim String

-- | parts for interpolation
newtype NamePart = NamePart String

newtype CName = CName [NamePart]

sname :: String -> CName
sname s = CName [NamePart s]

renderCName :: CName -> String
renderCName (CName ps) = intercalate "##" $ map (\(NamePart p) -> p) ps

data CExp = CEVerbatim String

data CFunDecl =
    CFunDecl CType CName [(CType,CName)] -- ^ type func( type1 arg1, type2 arg2, ... )

data CVarDecl =
    CVarDecl CType CName                 -- ^ type var

data CStatement =
    UsingNamespace Namespace -- ^ using namespace <namespace>;
  | TypeDef CType CName      -- ^ typedef origtype newname;
  | CDeclaration CFunDecl    -- ^ function declaration;
  | CDefinition CFunDecl [CStatement] -- ^ function definition;
  | CInit CVarDecl CExp      -- ^ variable initialization;
  | CReturn CExp             -- ^ return statement;
  | CDelete CExp             -- ^ delete statement;
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

renderCType :: CType -> String
renderCType (CTVerbatim t) = t

renderCExp :: CExp -> String
renderCExp (CEVerbatim e) = e

renderCFDecl :: CFunDecl -> String
renderCFDecl (CFunDecl typ fname args) =
    renderCType typ <> " " <> renderCName fname <> " ( " <> intercalate ", " (map mkArgStr args) <> " )"
  where
    mkArgStr (t, a) = renderCType t <> " " <> renderCName a

renderCVDecl :: CVarDecl -> String
renderCVDecl (CVarDecl typ vname) = renderCType typ <> " " <> renderCName vname

-- | render CStatement in a regular environment
renderCStmt :: CStatement -> String
renderCStmt (UsingNamespace (NS ns)) = "using namespace " <> ns <> ";"
renderCStmt (TypeDef typ n)          = "typedef " <> renderCType typ <> " " <> renderCName n <> ";"
renderCStmt (CDeclaration e)         = renderCFDecl e <> ";"
renderCStmt (CDefinition d body)     =
  renderCFDecl d <> " {\n" <> concatMap renderCStmt body <> "\n}\n"
renderCStmt (CInit d e)              = renderCVDecl d <> "=" <> renderCExp e <> ";"
renderCStmt (CReturn e)              = "return " <> renderCExp e <> ";"
renderCStmt (CDelete e)              = "delete " <> renderCExp e <> ";"
renderCStmt (CMacroApp n as)         = renderCName n <> "(" <> intercalate ", " (map renderCName as) <> ")" -- NOTE: no semicolon.
renderCStmt (Comment str)            = "// " <> str <> "\n"
renderCStmt CEmptyLine               = "\n"
renderCStmt (CVerbatim str)          = str

-- | render CStatement in a macro definition environment
renderCStmtInMacro :: CStatement -> [String]
renderCStmtInMacro (CDefinition d body) =
     [ renderCFDecl d <> " {" ]
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
