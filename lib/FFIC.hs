{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module FFIC where 

import Control.Applicative hiding (empty)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State hiding (mapM_)
import Data.Foldable (mapM_)
import Data.List (intercalate)
import Data.Sequence (Seq(..),(<|),(|>),empty,singleton)
import qualified Data.Sequence as S (reverse,null)
-- 
import Prelude hiding (mapM_)


-------------------------
-- some utilities here -- 
-------------------------

indent :: Int -> String -> String 
indent n =  unlines . map (replicate n ' ' ++ ) . lines  



data Primitive = PrimChar 
               | PrimInt 
               | PrimLong 
               | PrimUChar 
               | PrimULong 
               | PrimLongLong 
               | PrimULongLong 
               | PrimDouble 
               | PrimLongDouble 
               | PrimBool 
               | PrimVoid 
                deriving Show 

data Simple c = SPrim Primitive
              | SOpaq c 

deriving instance (Show c) => Show (Simple c)

-- | type representing full derived structure
data Composite c = CPtr (Composite c) 
                 | CRef (Composite c) 
                 | CSimple (Simple c) 

deriving instance (Show c) => Show (Composite c)

-- | type for simplifying class, class reference and class pointer
data Projected c = PPtr (Projected c) 
                 | PSimple (Simple c)

deriving instance (Show c) => Show (Projected c)


data Function t = Function { funcName :: String
                             , funcArgs :: [Var t] 
                             , funcRet  :: t }
  

deriving instance Functor Function 

data ConversionPrimitive c = ReinterpretCast c
                           | ApplyStar
                           | ApplyAmp
                           deriving Show 

-- deriving instance (Show c) => Show (ConversionPrim c)

-- type Conversion c = [ConvPrim c] -> [ConvPrim c]  

 
-- | project composite type to projected type
{-
project :: (Functor m, Monad m) => 
           Composite c -> StateT (Seq (ConversionPrimitive c)) m (Projected c) 
project (CSimple (SPrim x)) = pure (PSimple (SPrim x))
project (CSimple (SOpaq x)) = modify ( (|> ApplyStar) .  (|> ReinterpretCast x) ) *> pure (PSimple (SOpaq x))
project (CPtr (CSimple (SOpaq x))) = modify (ReinterpretCast x <|) *> pure (PSimple (SOpaq x))
project (CPtr x) = modify ( ApplyStar <|) >> PPtr <$> project x
project (CRef x) = modify ( id ) *> project (CPtr x) 
-}

project :: Composite c -> (Projected c, Maybe (Seq (ConversionPrimitive c))) 
project (CSimple (SPrim x)) = (PSimple (SPrim x), Nothing)
project (CSimple (SOpaq x)) = (PSimple (SOpaq x), Just (empty |> ReinterpretCast x |> ApplyStar))
project (CPtr (CSimple (SOpaq x))) = (PSimple (SOpaq x), Just (singleton (ReinterpretCast x)))
project (CPtr x) = let (p,s) = project x in (PPtr p, liftM ((ApplyStar <|) . (|> ApplyAmp)) s)
project (CRef x) = let (p,s) = project (CPtr x) in (PPtr p, liftM (|> ApplyStar) s)




-- | type with both before and after conversion  
data CompProjPair c = CPPair { cp_before :: Composite c
                             , cp_after :: Projected c 
                             , cp_conv :: Seq (ConversionPrimitive c) } 

mkCPPair :: Composite c -> CompProjPair c 
mkCPPair x = let (t,mc) = project x
             in CPPair x t (maybe empty id mc)



-- | 
mkCTypeFromPrimitive :: Primitive -> String 
mkCTypeFromPrimitive PrimChar       = "char"
mkCTypeFromPrimitive PrimInt        = "int"
mkCTypeFromPrimitive PrimLong       = "long"
mkCTypeFromPrimitive PrimUChar      = "unsigned char"
mkCTypeFromPrimitive PrimULong      = "unsigned long"
mkCTypeFromPrimitive PrimLongLong   = "long long"
mkCTypeFromPrimitive PrimULongLong  = "unsigned long long"
mkCTypeFromPrimitive PrimDouble     = "double"
mkCTypeFromPrimitive PrimLongDouble = "long double"
mkCTypeFromPrimitive PrimBool       = "bool"
mkCTypeFromPrimitive PrimVoid       = "void"

-- | 
mkCTypeFromComposite :: Composite String -> String
mkCTypeFromComposite (CSimple (SPrim p)) = mkCTypeFromPrimitive p 
mkCTypeFromComposite (CSimple (SOpaq x)) = x 
mkCTypeFromComposite (CPtr x) = "(" ++ mkCTypeFromComposite x ++ "*)"
mkCTypeFromComposite (CRef x) = "(" ++ mkCTypeFromComposite x ++ "&)"

-- | 
mkCTypeFromProjected :: Projected String -> String
mkCTypeFromProjected (PSimple (SPrim p)) = mkCTypeFromPrimitive p 
mkCTypeFromProjected (PSimple (SOpaq x)) = x ++ "_p" 
mkCTypeFromProjected (PPtr x) = "(" ++  mkCTypeFromProjected x ++ "*)"


-- | 
data Var t = Var { varType :: t, varName :: String }

deriving instance Functor Var 

-- | 
class Nameable a where 
  name :: a -> String


-- |
class CTypeable a where
  mkCType :: a -> String 

-- | 
class CallArguable a where 
  mkCallArg :: a -> String 


instance CTypeable (Composite String) where 
  mkCType = mkCTypeFromComposite    

instance CTypeable (Projected String) where
  mkCType = mkCTypeFromProjected 

instance CTypeable Primitive where
  mkCType = mkCTypeFromPrimitive 

instance CallArguable (Var (CompProjPair String)) where 
  mkCallArg v = let t = varType v 
                    n = varName v
                in mkConvStr n t 
               

instance Nameable String where 
  name = show 

-- | make conversion from Composite to Projected
mkConvStr :: String -> CompProjPair String -> String 
mkConvStr varname p = let r = cp_conv p  
                      in execState (mapM_ (modify . flip mkConvStrFromConvPrim) r) varname  

mkConvStrFromConvPrim :: String -> ConversionPrimitive String -> String 
mkConvStrFromConvPrim s ApplyStar = "(*" ++ s ++ ")"
mkConvStrFromConvPrim s ApplyAmp  = "(&" ++ s ++ ")"
mkConvStrFromConvPrim s (ReinterpretCast c) = "(reinterpret_cast<" ++ c ++ "*> " ++ s ++ ")"


-- | make conversion from Projected to Composite
mkRevConvStr :: String -> CompProjPair String -> String
mkRevConvStr varname p = let r = (S.reverse . cp_conv) p 
                         in execState (mapM_ (modify . flip mkRevConvStrFromConvPrim) r) varname

mkRevConvStrFromConvPrim :: String -> ConversionPrimitive String -> String 
mkRevConvStrFromConvPrim s ApplyStar = "(&" ++ s ++ ")"
mkRevConvStrFromConvPrim s ApplyAmp = "(*" ++ s ++ ")"
mkRevConvStrFromConvPrim s (ReinterpretCast c) = "(reinterpret_cast<" ++ c ++ "_t*> " ++ s ++ ")"



-- | construct a declaration statement from variable type and name pair
mkVarDecl :: (CTypeable t) => Var t -> String
mkVarDecl v = (mkCType . varType) v ++ " " ++ (varName v)

-- | construct argument string 
mkArgs :: (CTypeable t) => [Var t] -> String 
mkArgs = intercalate ", " . map mkVarDecl 

-- | construct call argument string
mkCallArgs :: CallArguable v => [v] -> String 
mkCallArgs = intercalate ", " . map mkCallArg 


-- | function declaration
mkFuncDecl :: (CTypeable t) => Function t -> String 
mkFuncDecl f = let retstr = (mkCType . funcRet) f 
                   argstr = (mkArgs . funcArgs) f 
               in retstr ++ " " ++ (funcName f) ++ "(" ++ argstr ++ ")" 

-- | 
mkFuncCallStr :: Function (CompProjPair String) -> String 
mkFuncCallStr f = let argstr = (mkCallArgs . funcArgs) f 
                      rettypbef = (cp_before . funcRet) f 
                      rettypaft = (cp_after . funcRet) f 
                      retconv = (cp_conv . funcRet) f 
                      callstr = funcName f ++ "(" ++ argstr ++ ")"
                      str = if (S.null retconv) 
                            then "return " ++ callstr  
                            else mkVarDecl (Var rettypbef "retval_b")  
                                 ++ ";\n"
                                 ++ mkVarDecl (Var rettypaft  "retval_a") 
                                 ++ ";\n"
                                 ++ "retval_a = " ++ callstr ++ ";\n"
                                 ++ "retval_b = " ++ mkRevConvStr "retval_a" (funcRet f)
                                 ++ ";\n"
                                 ++ "return retval_b;"  
                  in str


-- | 
mkFuncDef :: Function (CompProjPair String) -> String 
mkFuncDef f = let f_after = fmap (cp_after) f 
              in mkFuncDecl f_after ++ " {\n" ++ indent 2 (mkFuncCallStr f) ++ "}"


-- | opaqueification string 
mkOpaqueTypedef :: String -> [String] 
mkOpaqueTypedef name = let tag = name ++ "_tag" 
                           optyp = name ++ "_t"
                           opptr = name ++ "_p" 
                           opcptr = "const_" ++ name ++ "_p"
                       in [ "typedef struct " ++ tag ++ " " ++ optyp 
                          , "typedef " ++ optyp++"* " ++ opptr
                          , "typedef " ++ optyp++" const* " ++ opcptr ]

--
-- Now we need to have some test
-- 
-- first, if C++ has type (A&) 
--  then projected C type must be A_p = A_t* (A_t is opaque type)
-- C++ : A -> projected : again A_p 
-- C++ : A*  -> projected : again A_p 
-- C++ : A** -> projected : (A_p)*

-- in test/test.hs I am testing the above. 
-- the type (Composite c) is C++ type
-- the type (Projected c) is C type 
-- conversion is done in project. 
-- project is a state monad action since I need to keep what actual conversion I did during the whole conversion
-- test1: CSimple (SOpaq "A") -> PSimple (SOpaq "A")  : right... PSimple (SOpaq "A") = A_p 
-- I will never use A_t. so A_p is atomic. 
-- test2: CPtr (CSimple (SOpaq "A")) -> PSimple (SOpaq "A") : right .   (A* -> A_p)
-- test3: Cptr (CPtr (CSimple (SOpaq "A"))) -> PPtr (PSimple (SOpaq "A")) : right (A** -> A_p* ) 


-- now I am testing reference type
-- A& must go to A_p 
-- (A&)* must go to A_p*
-- test4 : CRef (CSimple (SOpaq "A")) -> PSimple (SOpaq "A") : right (A& -> A_p)
-- test5 : CPtr (CRef (CSimple (SOpaq "A"))) -> PPtr (PSimple (SOpaq "A")) : right  (A& -> A_p*)
-- (A*)& must go to A_p*
-- test6 : CRef (CPtr (CSimple (SOpaq "A"))) -> PPtr (PSimple (SOpaq "A")) : rignt ((A*)& -> A_p*)

-- now test with declaration .
-- composite type + varname versus projected type + var name
-- I introduce (Var t) type which is just pair of (t, String) 
-- 
-- mkVarDecl construct (type varname)   like int x, or A* p   something like that. 
-- reuse the test of test1
-- test1: A -> A_p : A x ->   A_p x 
-- test2: A* -> A_p : (A*) x -> A_p x
-- test3: A** -> A_p* : ((A*)*) x -> (A_p*) x 
-- test4: A& -> A_p : (A&) x -> A_p x  
-- ....


-- test function declaration. 
-- test7: works well

-- conversion 
-- now if I have A*  on original side, I have A_p on projected side. 
-- 
-- typedef struct A_tag A_t;
-- typedef A_t* A_p; 
-- void test(A_p x, int y) { 
--   return test(reinterpret_cast<A*> x, y ) ; 
-- } 
-- now more difficult examples 
-- (A**) -> (A_p)*
-- then      &(reinterpret_cast<A*>(*x))




