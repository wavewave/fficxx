{-# LANGUAGE TemplateHaskell #-}
module FFICXX.Runtime.Function.TH where
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import FFICXX.Runtime.TH
import FFICXX.Runtime.Function.Template


mkWrapper :: (Type,String) -> Q Dec
mkWrapper (typ,suffix)
  = do let fn = "wrap_" <> suffix
       n <- newName fn
       d <- forImpD CCall safe "wrapper" n [t| $(pure typ) -> IO (FunPtr ($(pure typ))) |]
       addTopDecls [d]
       pure $
         FunD (mkNameS "wrapFunPtr") [ Clause [] (NormalB (VarE n)) [] ]


t_newFunction :: Type -> String -> ExpQ
t_newFunction typ suffix
  = mkTFunc (typ, suffix, \ n -> "Function_new_" <> n, tyf)
  where tyf _n =
          let t = pure typ
          in [t| FunPtr $( t ) -> IO (Function $( t )) |]

t_call :: Type -> String -> ExpQ
t_call typ suffix
  = mkTFunc (typ, suffix, \ n -> "Function_call_" <> n, tyf)
  where tyf _n =
          let t = pure typ
          in [t| Function $( t ) -> $( t ) |]

t_deleteFunction :: Type -> String -> ExpQ
t_deleteFunction typ suffix
  = mkTFunc (typ, suffix, \ n -> "Function_delete_" <> n, tyf)
  where tyf _n =
          let t = pure typ
          in [t| Function $( t ) -> IO () |]


genFunctionInstanceFor :: Q Type -> String -> Q [Dec]
genFunctionInstanceFor qtyp suffix
  = do typ <- qtyp
       f1 <- mkNew "newFunction" t_newFunction typ suffix
       f2 <- mkMember "call" t_call typ suffix
       f3 <- mkMember "deleteFunction" t_deleteFunction typ suffix
       wrap <- mkWrapper (typ,suffix)
       let lst = [f1,f2,f3]
       return [ mkInstance [] (AppT (con "IFunction") typ) lst
              , mkInstance [] (AppT (con "FunPtrWrapper") typ) [wrap]
              ]
