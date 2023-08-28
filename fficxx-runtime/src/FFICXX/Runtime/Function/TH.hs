{-# LANGUAGE TemplateHaskell #-}

module FFICXX.Runtime.Function.TH where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
--
import FFICXX.Runtime.CodeGen.Cxx (CMacro (..), CStatement (..), renderCMacro, renderCStmt)
import FFICXX.Runtime.Function.Template (Function)
import FFICXX.Runtime.TH
  ( FunctionParamInfo (..),
    con,
    mkDelete,
    mkInstance,
    mkMember,
    mkNew,
    mkTFunc,
  )
import FFICXX.Runtime.Types (FFISafety (..))
import Foreign.Ptr (FunPtr)
import Language.Haskell.TH (forImpD, safe)
import Language.Haskell.TH.Syntax
  ( Body (NormalB),
    Callconv (CCall),
    Clause (..),
    Dec (..),
    Exp (..),
    ForeignSrcLang (LangCxx),
    Q,
    Type (..),
    addForeignSource,
    addModFinalizer,
    addTopDecls,
    mkNameS,
    newName,
  )

mkWrapper :: (Type, String) -> Q Dec
mkWrapper (typ, suffix) =
  do
    let fn = "wrap_" <> suffix
    n <- newName fn
    d <- forImpD CCall safe "wrapper" n [t|$(pure typ) -> IO (FunPtr ($(pure typ)))|]
    addTopDecls [d]
    pure $
      FunD (mkNameS "wrapFunPtr") [Clause [] (NormalB (VarE n)) []]

t_newFunction :: Type -> String -> Q Exp
t_newFunction typ suffix =
  mkTFunc FFIUnsafe (typ, suffix, \n -> "Function_new_" <> n, tyf)
  where
    tyf _n =
      let t = pure typ
       in [t|FunPtr $(t) -> IO (Function $(t))|]

t_call :: FFISafety -> Type -> String -> Q Exp
t_call safety typ suffix =
  mkTFunc safety (typ, suffix, \n -> "Function_call_" <> n, tyf)
  where
    tyf _n =
      let t = pure typ
       in [t|Function $(t) -> $(t)|]

t_deleteFunction :: Type -> String -> Q Exp
t_deleteFunction typ suffix =
  mkTFunc FFIUnsafe (typ, suffix, \n -> "Function_delete_" <> n, tyf)
  where
    tyf _n =
      let t = pure typ
       in [t|Function $(t) -> IO ()|]

genFunctionInstanceFor :: Q Type -> FunctionParamInfo -> Q [Dec]
genFunctionInstanceFor qtyp param =
  do
    let suffix = fpinfoSuffix param
    typ <- qtyp
    f1 <- mkNew "newFunction" t_newFunction typ suffix
    -- NOTE: The indirected function call should be safe.
    f2 <- mkMember "call" (t_call FFISafe) typ suffix
    f3 <- mkDelete "deleteFunction" t_deleteFunction typ suffix
    wrap <- mkWrapper (typ, suffix)
    addModFinalizer
      ( addForeignSource
          LangCxx
          ( "\n#include \"functional\"\n\n\n#include \"Function.h\"\n\n"
              ++ ( let headers = fpinfoCxxHeaders param
                       f x = renderCMacro (Include x)
                    in concatMap f headers
                 )
              ++ ( let nss = fpinfoCxxNamespaces param
                       f x = renderCStmt (UsingNamespace x)
                    in concatMap f nss
                 )
              ++ ( let retstr = fromMaybe "void" (fpinfoCxxRetType param)
                       argstr =
                         let args = fpinfoCxxArgTypes param
                             vs = case args of
                               [] -> "(,)"
                               _ ->
                                 intercalate "," $
                                   map (\(t, x) -> "(" ++ t ++ "," ++ x ++ ")") args
                          in "(" ++ vs ++ ")"
                    in "Function(" ++ suffix ++ "," ++ retstr ++ "," ++ argstr ++ ")\n"
                 )
          )
      )
    let lst = [f1, f2, f3]
    return
      [ mkInstance [] (AppT (con "IFunction") typ) lst,
        mkInstance [] (AppT (con "FunPtrWrapper") typ) [wrap]
      ]
