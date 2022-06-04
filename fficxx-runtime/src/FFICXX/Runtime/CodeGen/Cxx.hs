{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module FFICXX.Runtime.CodeGen.Cxx where

import Data.Functor.Identity (Identity)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.String (IsString (..))

newtype HeaderName = HdrName {unHdrName :: String}
  deriving (Hashable, Show, Eq, Ord)

instance IsString HeaderName where
  fromString = HdrName

newtype Namespace = NS {unNamespace :: String}
  deriving (Show, Eq, Ord)

instance IsString Namespace where
  fromString = NS

data PragmaParam = Once

-- | parts for interpolation
newtype NamePart (f :: * -> *) = NamePart String

newtype CName (f :: * -> *) = CName [NamePart f]

sname :: String -> CName Identity
sname s = CName [NamePart s]

renderCName :: CName Identity -> String
renderCName (CName ps) = intercalate "##" $ map (\(NamePart p) -> p) ps

-- | Types
data CType (f :: * -> *)
  = CTVoid
  | CTSimple (CName f)
  | CTStar (CType f)
  | CTAuto
  | CTTApp -- template type T<t1,t2,..>
      (CName f)
      -- ^ template type name
      [CType f]
      -- ^ template parameters
  | CTConst (CType f)
  | CTScoped (CType f) (CType f) -- some_class::inner_class
  -- TODO: refine this by restriction
  | CTVerbatim String

-- | Operators
data COp = CArrow | CAssign

renderCOp :: COp -> String
renderCOp CArrow = "->"
renderCOp CAssign = "="

data CExp (f :: * -> *)
  = -- | variable
    CVar (CName f)
  | -- | C function app:  f(a1,a2,..)
    CApp (CExp f) [CExp f]
  | -- | template app  :  f<T1,T2,..>(a1,a2,..)
    CTApp (CName f) [CType f] [CExp f]
  | -- | binary operator: x `op` y
    CBinOp COp (CExp f) (CExp f)
  | -- | (type)exp
    CCast (CType f) (CExp f)
  | -- | &(exp)
    CAddr (CExp f)
  | -- | *(exp)
    CStar (CExp f)
  | -- | new operator: new Cstr(a1,a2,...)
    CNew (CName f) [CExp f]
  | -- | new operator for template class: new Cstr<T1,T2,..>(a1,a2,..)
    CTNew (CName f) [CType f] [CExp f]
  | -- | new operator for inner class of template class: new Cstr<T1,T2,..>::inner(a1,a2,..) -- TODO: make a generalization
    CTNewI (CName f) (CName f) [CType f] [CExp f]
  | -- | macro function at expression level
    CEMacroApp (CName f) [CName f]
  | -- | verbatim
    CEVerbatim String
  | -- | empty C expression. (for convenience)
    CNull

data CFunDecl (f :: * -> *)
  = -- | type func( type1 arg1, type2 arg2, ... )
    CFunDecl (CType f) (CName f) [(CType f, CName f)]

data CVarDecl (f :: * -> *)
  = CVarDecl
      (CType f)
      -- ^ type
      (CName f)
      -- ^ variable name

data CQual = Inline

data CStatement (f :: * -> *)
  = -- | using namespace <namespace>;
    UsingNamespace Namespace
  | -- | typedef origtype newname;
    TypeDef (CType f) (CName f)
  | -- | C expression standalone;
    CExpSA (CExp f)
  | -- | function declaration;
    CDeclaration (CFunDecl f)
  | -- | function definition;
    CDefinition (Maybe CQual) (CFunDecl f) [CStatement f]
  | -- | variable initialization;
    CInit (CVarDecl f) (CExp f)
  | -- | return statement;
    CReturn (CExp f)
  | -- | delete statement;
    CDelete (CExp f)
  | -- | C Macro application at statement level (temporary)
    CMacroApp (CName f) [CName f]
  | -- | extern "C" {..}
    CExtern [CStatement f]
  | -- | comment
    Comment String
  | -- | for convenience
    CEmptyLine
  | -- | temporary verbatim
    CVerbatim String

data CMacro (f :: * -> *)
  = -- | regular C++ statement
    CRegular (CStatement f)
  | -- | #include "<header>"
    Include HeaderName
  | -- | #pragma
    Pragma PragmaParam
  | -- | #undef name
    Undef (CName f)
  | -- | #define macro (type) definition
    Define (CName f) [CName f] [CStatement f]
  | -- | just for convenience
    EmptyLine
  | -- | temporary verbatim
    Verbatim String

data CBlock (f :: * -> *) = ExternC [CMacro f] -- extern "C" with #ifdef __cplusplus guard.

renderPragmaParam :: PragmaParam -> String
renderPragmaParam Once = "once"

renderCType :: CType Identity -> String
renderCType CTVoid = "void"
renderCType (CTSimple n) = renderCName n
renderCType (CTStar t) = renderCType t <> "*"
renderCType CTAuto = "auto"
renderCType (CTTApp n ts) = renderCName n <> "<" <> intercalate ", " (map renderCType ts) <> ">"
renderCType (CTConst t) = "const " <> renderCType t
renderCType (CTScoped t i) = renderCType t <> "::" <> renderCType i
renderCType (CTVerbatim t) = t

renderCExp :: CExp Identity -> String
renderCExp (CVar n) = renderCName n
renderCExp (CApp f es) =
  ( case f of
      CVar _ -> renderCExp f
      _ -> "(" <> renderCExp f <> ")" -- compound expression like (*p)
  )
    <> "("
    <> intercalate ", " (map renderCExp es) -- arguments
    <> ")"
renderCExp (CTApp f ts es) =
  renderCName f
    <> "<"
    <> intercalate ", " (map renderCType ts) -- type arguments
    <> ">"
    <> "("
    <> intercalate ", " (map renderCExp es) -- arguments
    <> ")"
renderCExp (CBinOp o x y) =
  ( case x of
      CVar _ -> renderCExp x
      _ -> "(" <> renderCExp x <> ")" -- compound expression like (*p)
  )
    <> renderCOp o
    <> renderCExp y
renderCExp (CCast t e) = "(" <> renderCType t <> ")" <> renderCExp e
renderCExp (CAddr e) = "&(" <> renderCExp e <> ")"
renderCExp (CStar e) = "*(" <> renderCExp e <> ")"
renderCExp (CNew n es) =
  "new "
    <> renderCName n -- constructor name
    <> "("
    <> intercalate ", " (map renderCExp es) -- arguments
    <> ")"
renderCExp (CTNew n ts es) =
  "new "
    <> renderCName n -- constructor name
    <> "<"
    <> intercalate ", " (map renderCType ts) -- type arguments
    <> ">"
    <> "("
    <> intercalate ", " (map renderCExp es) -- arguments
    <> ")"
renderCExp (CTNewI n i ts es) =
  "new "
    <> renderCName n -- constructor name
    <> "<"
    <> intercalate ", " (map renderCType ts) -- type arguments
    <> ">::"
    <> renderCName i -- inner class name
    <> "("
    <> intercalate ", " (map renderCExp es) -- arguments
    <> ")"
renderCExp (CEMacroApp n as) =
  renderCName n
    <> "("
    <> intercalate ", " (map renderCName as)
    <> ")" -- NOTE: no semicolon.
renderCExp (CEVerbatim e) = e
renderCExp CNull = ""

renderCQual :: CQual -> String
renderCQual Inline = "inline"

renderCFDecl :: CFunDecl Identity -> String
renderCFDecl (CFunDecl typ fname args) =
  renderCType typ <> " " <> renderCName fname <> " ( " <> intercalate ", " (map mkArgStr args) <> " )"
  where
    mkArgStr (t, a) = renderCType t <> " " <> renderCName a

renderCVDecl :: CVarDecl Identity -> String
renderCVDecl (CVarDecl typ vname) = renderCType typ <> " " <> renderCName vname

-- | render CStatement in a regular environment
renderCStmt :: CStatement Identity -> String
renderCStmt (UsingNamespace (NS ns)) = "using namespace " <> ns <> ";"
renderCStmt (TypeDef typ n) = "typedef " <> renderCType typ <> " " <> renderCName n <> ";"
renderCStmt (CExpSA e) = renderCExp e <> ";"
renderCStmt (CDeclaration e) = renderCFDecl e <> ";"
renderCStmt (CDefinition mq d body) =
  maybe "" (\q -> renderCQual q <> " ") mq
    <> renderCFDecl d
    <> " {\n"
    <> concatMap renderCStmt body
    <> "\n}\n"
renderCStmt (CInit d e) = renderCVDecl d <> "=" <> renderCExp e <> ";"
renderCStmt (CReturn e) = "return " <> renderCExp e <> ";"
renderCStmt (CDelete e) = "delete " <> renderCExp e <> ";"
renderCStmt (CMacroApp n as) =
  renderCName n
    <> "("
    <> intercalate ", " (map renderCName as)
    <> ")" -- NOTE: no semicolon.
renderCStmt (CExtern body) =
  "extern \"C\" {\n"
    <> concatMap renderCStmt body
    <> "}\n"
renderCStmt (Comment str) = "// " <> str <> "\n"
renderCStmt CEmptyLine = "\n"
renderCStmt (CVerbatim str) = str

-- | render CStatement in a macro definition environment
renderCStmtInMacro :: CStatement Identity -> [String]
renderCStmtInMacro (Comment _str) = [""] -- Comment cannot exist in Macro
renderCStmtInMacro CEmptyLine = [""]
renderCStmtInMacro (CVerbatim str) = lines str
renderCStmtInMacro s = lines (renderCStmt s)

renderCMacro :: CMacro Identity -> String
renderCMacro (CRegular stmt) = renderCStmt stmt
renderCMacro (Include (HdrName hdr)) = "\n#include \"" <> hdr <> "\"\n"
renderCMacro (Pragma param) = "\n#pragma " <> renderPragmaParam param <> "\n"
renderCMacro (Undef n) = "\n#undef " <> renderCName n <> "\n"
renderCMacro (Define m ts stmts) =
  "\n#define " <> renderCName m
    <> case ts of
      [] -> " "
      _ -> "(" <> intercalate ", " (map renderCName ts) <> ") \\\n"
    <> intercalate "\\\n" (concatMap renderCStmtInMacro stmts)
    <> "\n"
renderCMacro EmptyLine = "\n"
renderCMacro (Verbatim str) = str

renderBlock :: CBlock Identity -> String
renderBlock (ExternC ms) =
  "\n#ifdef __cplusplus\n\
  \extern \"C\" {\n\
  \#endif\n"
    ++ concatMap renderCMacro ms
    ++ "\n#ifdef __cplusplus\n\
       \}\n\
       \#endif\n"
