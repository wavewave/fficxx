{-# LANGUAGE TemplateHaskell #-}

module FFICXX.Runtime.TH where

import Language.Haskell.TH        ( forImpD, safe, varE )
import Language.Haskell.TH.Syntax ( Body(NormalB), Callconv(CCall)
                                  , Clause(..), Cxt, Dec(..)
                                  , Exp(..), Pat(..), Q, Type(..)
                                  , addTopDecls, mkNameS, newName
                                  )
--
import FFICXX.Runtime.CodeGen.Cxx ( HeaderName, Namespace)


-- | Primitive C type like int, double should be treated differently than
--   Non-primitive type. The primitive type detection is not yet automatic.
--   So we manually mark template instantiation with this boolean parameter.
data IsCPrimitive =
    CPrim
  | NonCPrim
  deriving Show


-- | template parameter: A,B,.. in T<A,B..>
data TemplateParamInfo =
  TPInfo {
    tpinfoCxxType       :: String
  -- , tpinfoIsCPrimitive  :: IsCPrimitive  -- ^ whether the parameter is C-primitive type
  , tpinfoCxxHeaders    :: [HeaderName]
  , tpinfoCxxNamespaces :: [Namespace]
  , tpinfoSuffix        :: String
  }
  deriving Show

-- | function pointer parameter A(B,C,..) in std::function<A(B,C,..)>
data FunctionParamInfo =
  FPInfo {
    fpinfoCxxArgTypes   :: [(String,String)]
  , fpinfoCxxRetType    :: Maybe String
  , fpinfoCxxHeaders    :: [HeaderName]
  , fpinfoCxxNamespaces :: [Namespace]
  , fpinfoSuffix        :: String
  }
  deriving Show

con :: String -> Type
con = ConT . mkNameS

-- |
mkInstance :: Cxt -> Type -> [Dec] -> Dec
mkInstance = InstanceD Nothing

-- |
mkTFunc :: (types, String, String -> String, types -> Q Type) -> Q Exp
mkTFunc (typs, suffix, nf, tyf)
  = do let fn = nf suffix
       let fn' = "c_" <> fn
       n <- newName fn'
       d <- forImpD CCall safe fn n (tyf typs)
       addTopDecls [d]
       [| $( varE n ) |]

-- |
mkMember :: String -> (types -> String -> Q Exp) -> types -> String -> Q Dec
mkMember fname f typ suffix = do
  let x = mkNameS "x"
  e <- f typ suffix
  pure $
    FunD (mkNameS fname) [ Clause [VarP x] (NormalB (AppE e (VarE x))) [] ]

-- |
mkNew :: String -> (types -> String -> Q Exp) -> types -> String -> Q Dec
mkNew fname f typ suffix = do
  e <- f typ suffix
  pure $
    FunD (mkNameS fname)
      [ Clause [] (NormalB e) [] ]

-- |
mkDelete :: String -> (types -> String -> Q Exp) -> types -> String -> Q Dec
mkDelete = mkMember

-- |
mkFunc :: String -> (types -> String -> Q Exp) -> types -> String -> Q Dec
mkFunc fname f typ suffix = do
  let x = mkNameS "x"
  e <- f typ suffix
  pure $
    FunD (mkNameS fname) [ Clause [VarP x] (NormalB (AppE e (VarE x))) [] ]

-- | utility function for converting '.' to '_'
dot2_ :: String -> String
dot2_ = map (\c -> if c == '.' then '_' else c)
