{-# LANGUAGE RecordWildCards #-}
module FFICXX.Generate.Name where

import           Data.Char                         ( toLower )
import           Data.Maybe                        ( fromMaybe, maybe )
import           Data.Monoid                       ( (<>) )
--
import           FFICXX.Generate.Type.Class        ( Accessor(..)
                                                   , Arg(..)
                                                   , Class(..)
                                                   , ClassAlias(caHaskellName,caFFIName)
                                                   , Function(..)
                                                   , TemplateArgType(..)
                                                   , TemplateClass(..)
                                                   , TemplateFunction(..)
                                                   , TemplateMemberFunction(..)
                                                   , TopLevel(..)
                                                   , Variable(..)
                                                   )
import           FFICXX.Generate.Util              ( firstLower, toLowers )



hsFrontNameForTopLevel :: TopLevel -> String
hsFrontNameForTopLevel tfn =
    let (x:xs) = case tfn of
                   TopLevelFunction {..} -> fromMaybe toplevelfunc_name toplevelfunc_alias
                   TopLevelVariable {..} -> fromMaybe toplevelvar_name  toplevelvar_alias
    in toLower x : xs


typeclassName :: Class -> String
typeclassName c = 'I' : fst (hsClassName c)

typeclassNameT :: TemplateClass -> String
typeclassNameT c = 'I' : fst (hsTemplateClassName c)



typeclassNameFromStr :: String -> String
typeclassNameFromStr = ('I':)

hsClassName :: Class -> (String, String)  -- ^ High-level, 'Raw'-level
hsClassName c =
  let cname = maybe (class_name c) caHaskellName (class_alias c)
  in (cname, "Raw" <> cname)

hsClassNameForTArg :: TemplateArgType -> String
hsClassNameForTArg (TArg_Class c) = fst (hsClassName c)
hsClassNameForTArg (TArg_TypeParam p) = p
hsClassNameForTArg (TArg_Other s) = s

hsTemplateClassName :: TemplateClass -> (String, String)  -- ^ High-level, 'Raw'-level
hsTemplateClassName t =
  let tname = tclass_name t
  in (tname, "Raw" <> tname)

existConstructorName :: Class -> String
existConstructorName c = 'E' : (fst.hsClassName) c


ffiClassName :: Class -> String
ffiClassName c = maybe (class_name c) caFFIName (class_alias c)

hscFuncName :: Class -> Function -> String
hscFuncName c f =    "c_"
                  <> toLowers (ffiClassName c)
                  <> "_"
                  <> toLowers (aliasedFuncName c f)

hsFuncName :: Class -> Function -> String
hsFuncName c f = let (x:xs) = aliasedFuncName c f
                 in (toLower x) : xs

aliasedFuncName :: Class -> Function -> String
aliasedFuncName c f =
  case f of
    Constructor _ a      -> fromMaybe (constructorName c) a
    Virtual _ str _ a    -> fromMaybe str a
    NonVirtual _ str _ a -> fromMaybe (nonvirtualName c str) a
    Static _ str _ a     -> fromMaybe (nonvirtualName c str) a
    Destructor a         -> fromMaybe destructorName a

-- |
hsTmplFuncName :: TemplateClass -> TemplateFunction -> String
hsTmplFuncName t f =
  case f of
    TFun {..}    -> tfun_name
    TFunNew {..} -> fromMaybe ("new"<> tclass_name t) tfun_new_alias
    TFunDelete   -> "delete" <> tclass_name t
    TFunOp {..}  -> tfun_name

-- |
hsTmplFuncNameTH :: TemplateClass -> TemplateFunction -> String
hsTmplFuncNameTH t f = "t_" <> hsTmplFuncName t f


hsTemplateMemberFunctionName :: Class -> TemplateMemberFunction -> String
hsTemplateMemberFunctionName c f = fromMaybe (nonvirtualName c (tmf_name f)) (tmf_alias f)


hsTemplateMemberFunctionNameTH :: Class -> TemplateMemberFunction -> String
hsTemplateMemberFunctionNameTH c f = "t_" <> hsTemplateMemberFunctionName c f


ffiTmplFuncName :: TemplateFunction -> String
ffiTmplFuncName f =
  case f of
    TFun {..}    -> tfun_name
    TFunNew {..} -> fromMaybe "new" tfun_new_alias
    TFunDelete   -> "delete"
    TFunOp {..}  -> tfun_name

cppTmplFuncName :: TemplateFunction -> String
cppTmplFuncName f =
  case f of
    TFun {..}    -> tfun_name
    TFunNew {..} -> "new"
    TFunDelete   -> "delete"
    TFunOp {..}  -> tfun_name

-- |
accessorName :: Class -> Variable -> Accessor -> String
accessorName c v a =    nonvirtualName c (arg_name (unVariable v))
                     <> "_"
                     <> case a of
                          Getter -> "get"
                          Setter -> "set"

-- |
hscAccessorName :: Class -> Variable -> Accessor -> String
hscAccessorName c v a = "c_" <> toLowers (accessorName c v a)

-- |
tmplAccessorName :: Variable -> Accessor -> String
tmplAccessorName (Variable (Arg _ n)) a =
     n <> "_" <> case a of { Getter -> "get"; Setter -> "set" }

-- |
cppStaticName :: Class -> Function -> String
cppStaticName c f = class_name c <> "::" <> func_name f

-- |
cppFuncName :: Class -> Function -> String
cppFuncName c f =   case f of
    Constructor _ _ -> "new"
    Virtual _ _  _ _ -> func_name f
    NonVirtual _ _ _ _-> func_name f
    Static _ _ _ _-> cppStaticName c f
    Destructor _ -> destructorName

constructorName :: Class -> String
constructorName c = "new" <> (fst.hsClassName) c

nonvirtualName :: Class -> String -> String
nonvirtualName c str = (firstLower.fst.hsClassName) c <> "_" <> str

destructorName :: String
destructorName = "delete"

