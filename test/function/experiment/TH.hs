{-# LANGUAGE TemplateHaskell #-}
module TH where
import Data.Char
import Data.Monoid
import Foreign.C.Types
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import FFICXX.Runtime.TH
import Template


mkTFunc' :: (Type, String, String -> String, Type -> Q Type) -> ExpQ
mkTFunc' (typ, ncty, nf, tyf)
  = do let fn = nf ncty
       let fn' = "c_" <> fn
       n <- newName fn'
       -- let n = mkNameU fn'
       d <- forImpD CCall unsafe fn n (tyf typ)
       addTopDecls [d]
       [| $( varE n ) |]

mkNew' :: String -> (Type -> String -> Q Exp) -> Type -> String -> Q Dec
mkNew' fname f typ suffix = do
  e <- f typ suffix
  return $
    FunD (mkNameS fname)
      [ Clause [] (NormalB e) [] ]



t_newFunction :: Type -> String -> ExpQ
t_newFunction typ ncty
  = mkTFunc' (typ, ncty, \ n -> "Function_new_" <> n, tyf)
  where tyf n =
          let t = return typ
          in [t| FunPtr $( t ) -> IO (Function $( t )) |]

-- t_callFunction :: Name -> String -> ExpQ
-- t_callFunction = undefined

genFunctionInstanceFor :: Q Type -> String -> Q [Dec]
genFunctionInstanceFor qtyp suffix
  = do typ <- qtyp
       f1 <- mkNew' "newFunction" t_newFunction typ suffix
       let lst = [f1]
       return [mkInstance [] (AppT (con "IFunction") typ) lst]
