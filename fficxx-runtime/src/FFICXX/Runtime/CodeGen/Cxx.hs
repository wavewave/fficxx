{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module FFICXX.Runtime.CodeGen.Cxx where

import Data.Functor.Identity (Identity)
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

-- | Dummy ~ Identity. For testing now.
data Dummy a = Dummy { unDummy :: a }

-- data Op = Add | Mul

-- | embedded DSL for C++ code generation via interpretation
data HExp =
    Val Int
  | Add HExp HExp
  | Lam (HExp -> HExp)
  | App HExp HExp
--   deriving Show

-- data HEval =
--     EVal Int
--   | ELam String HEval

-- data Evaluated =
--     EValue   Int                  -- ^ fully evaluated
--   | EClosure (HExp -> Evaluated)  -- ^ closure

-- pprint :: Evaluated -> String
-- pprint (EValue n) = show n
-- pprint (EClosure f) = "closure"

eval :: HExp -> Int
eval (Val v)   = v
eval (Add x y) = eval x + eval y
eval (Lam _)   = error "lambda"
eval (App (Lam f) x) = eval (f x)


data PragmaParam = Once

-- | parts for interpolation
newtype NamePart (f :: * -> *) = NamePart String

newtype CName (f :: * -> *) = CName [NamePart f]

sname :: String -> CName Identity
sname s = CName [NamePart s]

renderCName :: CName Identity -> String
renderCName (CName ps) = intercalate "##" $ map (\(NamePart p) -> p) ps


data CType (f :: * -> *) =
    CTVoid
  | CTSimple (CName f)
  | CTStar (CType f)
  | CTAuto
  | CTVerbatim String


data COp = CArrow | CAssign

renderCOp :: COp -> String
renderCOp CArrow  = "->"
renderCOp CAssign = "="

data CExp (f :: * -> *) =
    CVar (CName f)                      -- ^ variable
  | CApp (CExp f) [CExp f]              -- ^ C function app:  f(a1,a2,..)
  | CTApp (CName f ) [CType f] [CExp f] -- ^ template app  :  f<T1,T2,..>(a1,a2,..)
  | CBinOp COp (CExp f) (CExp f)        -- ^ binary operator: x `op` y
  | CCast (CType f) (CExp f)            -- ^ (type)exp
  | CAddr (CExp f)                      -- ^ &(exp)
  | CStar (CExp f)                      -- ^ *(exp)
  | CNew (CName f) [CExp f]             -- ^ new operator: new Cstr(a1,a2,...)
  | CTNew (CName f) [CType f] [CExp f]  -- ^ new operator for template class: new Cstr<T1,T2,..>(a1,a2,..)
  | CEMacroApp (CName f) [CName f]      -- ^ macro function at expression level
  | CEVerbatim String                   -- ^ verbatim
  | CNull                               -- ^ empty C expression. (for convenience)

data CFunDecl (f :: * -> *) =
  CFunDecl (CType f) (CName f) [(CType f,CName f)] -- ^ type func( type1 arg1, type2 arg2, ... )

data CVarDecl (f :: * -> *) =
  CVarDecl
    (CType f)  -- ^ type
    (CName f)  -- ^ variable name

data CQual = Inline

data CStatement (f :: * -> *) =
    UsingNamespace Namespace                -- ^ using namespace <namespace>;
  | TypeDef (CType f) (CName f)             -- ^ typedef origtype newname;
  | CExpSA (CExp f)                         -- ^ C expression standalone;
  | CDeclaration (CFunDecl f)               -- ^ function declaration;
  | CDefinition (Maybe CQual) (CFunDecl f) [CStatement f]
                                            -- ^ function definition;
  | CInit (CVarDecl f) (CExp f)             -- ^ variable initialization;
  | CReturn (CExp f)                        -- ^ return statement;
  | CDelete (CExp f)                        -- ^ delete statement;
  | CMacroApp (CName f) [CName f]           -- ^ C Macro application at statement level (temporary)
  | CExtern [CStatement f]                  -- ^ extern "C" {..}
  | Comment String                          -- ^ comment
  | CEmptyLine                              -- ^ for convenience
  | CVerbatim String                        -- ^ temporary verbatim

data CMacro (f :: * -> *) =
    CRegular (CStatement f)                   -- ^ regular C++ statement
  | Include HeaderName                        -- ^ #include "<header>"
  | Pragma PragmaParam                        -- ^ #pragma
  | Undef (CName f)                           -- ^ #undef name
  | Define (CName f) [CName f] [CStatement f] -- ^ #define macro (type) definition
  | EmptyLine                                 -- ^ just for convenience
  | Verbatim String                           -- ^ temporary verbatim

data CBlock (f :: * -> *) = ExternC [CMacro f] -- extern "C" with #ifdef __cplusplus guard.

renderPragmaParam :: PragmaParam -> String
renderPragmaParam Once = "once"

renderCType :: CType Identity -> String
renderCType CTVoid         = "void"
renderCType (CTSimple n)   = renderCName n
renderCType (CTStar t)     = renderCType t <> "*"
renderCType CTAuto         = "auto"
renderCType (CTVerbatim t) = t

renderCExp :: CExp Identity -> String
renderCExp (CVar n)        = renderCName n
renderCExp (CApp f es)     =    (case f of
                                   CVar _ -> renderCExp f
                                   _      -> "(" <> renderCExp f <> ")" -- compound expression like (*p)
                                )
                             <> "("
                             <> intercalate ", " (map renderCExp es)  -- arguments
                             <> ")"
renderCExp (CTApp f ts es) =    renderCName f
                             <> "<"
                             <> intercalate ", " (map renderCType ts) -- type arguments
                             <> ">"
                             <> "("
                             <> intercalate ", " (map renderCExp es)  -- arguments
                             <> ")"
renderCExp (CBinOp o x y)  =    (case x of
                                   CVar _ -> renderCExp x
                                   _      -> "(" <> renderCExp x <> ")" -- compound expression like (*p)
                                )
                             <> renderCOp o
                             <> renderCExp y
renderCExp (CCast t e)     = "(" <> renderCType t <> ")" <> renderCExp e
renderCExp (CAddr e)       = "&(" <> renderCExp e <> ")"
renderCExp (CStar e)       = "*(" <> renderCExp e <> ")"
renderCExp (CNew n es)     =    "new "
                             <> renderCName n                         -- constructor name
                             <> "("
                             <> intercalate ", " (map renderCExp es)  -- arguments
                             <> ")"
renderCExp (CTNew n ts es) =    "new "
                             <> renderCName n                         -- constructor name
                             <> "<"
                             <> intercalate ", " (map renderCType ts) -- type arguments
                             <> ">"
                             <> "("
                             <> intercalate ", " (map renderCExp es)  -- arguments
                             <> ")"
renderCExp (CEMacroApp n as) =  renderCName n
                             <> "("
                             <> intercalate ", " (map renderCName as)
                             <> ")" -- NOTE: no semicolon.
renderCExp (CEVerbatim e)  = e
renderCExp CNull           = ""

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
renderCStmt (TypeDef typ n)          = "typedef " <> renderCType typ <> " " <> renderCName n <> ";"
renderCStmt (CExpSA e)               = renderCExp e <> ";"
renderCStmt (CDeclaration e)         = renderCFDecl e <> ";"
renderCStmt (CDefinition mq d body)  =    maybe "" (\q -> renderCQual q <> " ") mq
                                       <> renderCFDecl d
                                       <> " {\n"
                                       <> concatMap renderCStmt body
                                       <> "\n}\n"
renderCStmt (CInit d e)              = renderCVDecl d <> "=" <> renderCExp e <> ";"
renderCStmt (CReturn e)              = "return " <> renderCExp e <> ";"
renderCStmt (CDelete e)              = "delete " <> renderCExp e <> ";"
renderCStmt (CMacroApp n as)         =    renderCName n
                                       <> "("
                                       <> intercalate ", " (map renderCName as)
                                       <> ")" -- NOTE: no semicolon.
renderCStmt (CExtern body)           =   "extern \"C\" {\n"
                                       <> concatMap renderCStmt body
                                       <> "}\n"
renderCStmt (Comment str)            = "// " <> str <> "\n"
renderCStmt CEmptyLine               = "\n"
renderCStmt (CVerbatim str)          = str

-- | render CStatement in a macro definition environment
renderCStmtInMacro :: CStatement Identity -> [String]
renderCStmtInMacro (Comment _str)  = [""] -- Comment cannot exist in Macro
renderCStmtInMacro CEmptyLine      = [""]
renderCStmtInMacro (CVerbatim str) = lines str
renderCStmtInMacro s               = lines (renderCStmt s)

renderCMacro :: CMacro Identity -> String
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


renderBlock :: CBlock Identity -> String
renderBlock (ExternC ms) =
     "\n#ifdef __cplusplus\n\
     \extern \"C\" {\n\
     \#endif\n"
  ++ concatMap renderCMacro ms
  ++ "\n#ifdef __cplusplus\n\
     \}\n\
     \#endif\n"
