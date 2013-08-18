
-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Type.Class
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Type.Class where

import Control.Applicative ((<$>),(<*>))
import Data.Char
import Data.List
import qualified Data.Map as M
import System.FilePath
--
import FFICXX.Generate.Util
import FFICXX.Generate.Type.Internal

{-
  CPPType are represented by a base type (e.g., "int") and a collection of
  type operators applied to the base (e.g., pointers, arrays, etc...).

  Encoding:

  CPPType are encoded as strings of type constructors such as follows:

         String Encoding                 C Example
         ---------------                 ---------
         p.p.int                         int  *
         a(300).a(400).int               int [300][400]
         p.q(const).char                 char const

  All type constructors are denoted by a trailing '.':

   'p.'                = Pointer (*)
   'r.'                = Reference (&)
   'a(n).'             = Array of size n  [n]
   'f(..,..).'         = Function with arguments  (args)
   'q(str).'           = Qualifier (such as const or volatile) (const, volatile)
   'm(qual).'          = Pointer to member (qual::*)

  The encoding follows the order that you might describe a type in words.
  For example "p.a(200).int" is "A pointer to array of int's" and
  "p.q(const).char" is "a pointer to a const char".

  This representation of types is fairly convenient because ordinary string
  operations can be used for type manipulation. For example, a type could be
  formed by combining two strings such as the following:

         "p.p." + "a(400).int" = "p.p.a(400).int"

  Similarly, one could strip a 'const' declaration from a type doing something
  like this:

         Replace(t,"q(const).","",DOH_REPLACE_ANY)

  For the most part, this module tries to minimize the use of special
  characters (*, [, <, etc...) in its type encoding.  One reason for this
  is that SWIG might be extended to encode data in formats such as XML
  where you might want to do this:

       <function>
          <type>p.p.int</type>
          ...
       </function>

  Or alternatively,

       <function type="p.p.int" ...>blah</function>

  In either case, it's probably best to avoid characters such as '&', '*', or '<'.
-}


cvarToStr :: (CPPNameable c) => CPPType c -> String -> String
cvarToStr t varname = (cppname t) `connspace` varname


-- | return type for haskell-C interface. special treatment to C++-only type
rettypeToString :: (CPPNameable c) => CPPType c -> String
rettypeToString (PrimType (CPTClass c)) = cppname c ++ "_p"
rettypeToString (Ptr (PrimType (CPTClass c))) = cppname c ++ "_p"
rettypeToString (Ref (PrimType (CPTClass c))) = cppname c ++ "_p"
rettypeToString t = cppname t


-- | Convert a function argument type to C syntax string
argToString :: (CPPNameable c) => (CPPType c,String) -> String
argToString (t, varname) = rettypeToString t `connspace` varname

-- | function argument in C++ call
argToCallString :: (CPPNameable c) => (CPPType c,String) -> String
argToCallString (PrimType (CPTClass c),varname) =
    "to_nonconst<"++cppname c++","++cppname c++"_t>("++varname++")"
argToCallString (Ptr (PrimType (CPTClass c)),varname) =
    "to_nonconstref<"++cppname c++","++cppname c++"_t>(*"++varname++")"
argToCallString (_,varname) = varname

-- | Convert function names to lower case
hsFrontNameForTopLevelFunction :: TopLevelFunction c -> String
hsFrontNameForTopLevelFunction tfn =
    let (x:xs) = maybe (toplevelfunc_name tfn) id (toplevelfunc_alias tfn)
    in toLower x : xs

-- | Check if a member function is constructor
isNewFunc :: MethodMemberType c -> Bool
isNewFunc (Constructor _ _ _) = True
isNewFunc _ = False

-- | Check if a member function is a destructor
isDeleteFunc :: MethodMemberType c-> Bool
isDeleteFunc (Destructor _ _ _ _) = True
isDeleteFunc _ = False

-- | Check if a method is virtual, destructors are treated as virtual
isVirtualFunc :: MethodMemberType c -> Bool
isVirtualFunc (Destructor _ _ _ _) = True
isVirtualFunc (NormalMethod Virtual _ _ _ _) = True
isVirtualFunc _ = False

-- | Check if a method is static
isStaticFunc :: MethodMemberType c -> Bool
isStaticFunc (NormalMethod Static _ _ _ _) = True
isStaticFunc _ = False

-- | Check if a class member is a function member
isMethod :: ClassMember c -> Bool
isMethod (MethodMember _) = True
isMethod _                = False

-- | Check if a class member is a data member
isDataMember :: ClassMember c -> Bool
isDataMember (DataMember _) = True
isDataMember _              = False

-- | Get all function members from a class definition
classFuncs :: Class -> [MethodMemberType Class]
classFuncs cls = map unwrapMethod $ filter isMethod (class_members cls)
  where unwrapMethod (MethodMember m) = m
        unwrapMethod _ = error "Error: not a member function"

class_funcs ::  Class -> [MethodMemberType Class]
class_funcs = classFuncs

-- | Get all virtual functions
virtualFuncs :: [MethodMemberType c] -> [MethodMemberType c]
virtualFuncs = filter isVirtualFunc

-- | Get all consturctors from methods
constructorFuncs :: [MethodMemberType c] -> [MethodMemberType c]
constructorFuncs = filter isNewFunc

-- | Get all virtual methods
nonVirtualNotNewFuncs :: [MethodMemberType c] -> [MethodMemberType c]
nonVirtualNotNewFuncs =
  filter (\x -> (not.isVirtualFunc) x && (not.isNewFunc) x && (not.isDeleteFunc) x && (not.isStaticFunc) x )

-- | Get all static functions
staticFuncs :: [MethodMemberType c] -> [MethodMemberType c]
staticFuncs = filter isStaticFunc

--------

-- | Check abstract class

isAbstractClass :: Class -> Bool
isAbstractClass (Class _ _ _ _ _ _) = False
isAbstractClass (AbstractClass _ _ _ _ _ _) = True

class_allparents :: Class -> [Class]
class_allparents c = let ps = class_parents c
                     in  if null ps
                           then []
                           else nub (ps ++ (concatMap class_allparents ps))

getClassModuleBase :: Class -> String
getClassModuleBase = (<.>) <$> (cabal_moduleprefix.class_cabal) <*> (fst.hsClassName)


-- | Daughter map not including itself
mkDaughterMap :: [Class] -> DaughterMap
mkDaughterMap = foldl mkDaughterMapWorker M.empty
  where mkDaughterMapWorker m c = let ps = map getClassModuleBase (class_allparents c)
                                  in  foldl (addmeToYourDaughterList c) m ps
        addmeToYourDaughterList c m p = let f Nothing = Just [c]
                                            f (Just cs)  = Just (c:cs)
                                        in  M.alter f p m



-- | Daughter Map including itself as a daughter
mkDaughterSelfMap :: [Class] -> DaughterMap
mkDaughterSelfMap = foldl worker M.empty
  where worker m c = let ps = map getClassModuleBase (c:class_allparents c)
                     in  foldl (addToList c) m ps
        addToList c m p = let f Nothing = Just [c]
                              f (Just cs)  = Just (c:cs)
                          in  M.alter f p m

-- | Get the interface type class name from the high level class name in C++
typeclassName :: Class -> String
typeclassName c = 'I' : fst (hsClassName c)

typeclassNameFromStr :: String -> String
typeclassNameFromStr = ('I':)

-- | High-level, 'Raw'-level
hsClassName :: Class -> (String, String)
hsClassName c =
  let cname = maybe (class_name c) id (class_alias c)
  in (cname, "Raw" ++ cname)

existConstructorName :: Class -> String
existConstructorName c = 'E' : (fst.hsClassName) c

-- | this is for FFI
hsFuncTyp :: CPPNameable c => Class -> MethodMemberType c -> String
hsFuncTyp c f = let args = genericFuncArgs f
                    ret  = genericFuncRet f
                in selfstr ++ " -> " ++ concatMap ((++ " -> ") . hsargtype . fst) args ++ hsrettype ret

  where (_,rcname) = hsClassName c
        selfstr = "(Ptr " ++ rcname ++ ")"
        hsrettype r = "IO" ++ hsCPPTypeName r
        hsargtype :: CPPNameable c => CPPType c -> String
        hsargtype = hsCPPTypeName

hsFuncTypNoSelf :: CPPNameable c => Class -> MethodMemberType c -> String
hsFuncTypNoSelf _ f = let args = genericFuncArgs f
                          ret  = genericFuncRet f
                      in  intercalateWith connArrow id $ map (hsargtype . fst) args ++ [hsrettype ret]

  where --(_,rcname) = hsClassName c
        {-selfstr = "(Ptr " ++ rcname ++ ")"-}
        hsrettype r = "IO" ++ hsCPPTypeName r
        hsargtype = hsCPPTypeName

hscFuncName :: Class -> MethodMemberType c -> String
hscFuncName c f = "c_" ++ toLowers (class_name c) ++ "_" ++ toLowers (aliasedFuncName c f)

hsFuncName :: Class -> MethodMemberType c -> String
hsFuncName c f = let (x:xs) = aliasedFuncName c f
                 in (toLower x) : xs

hsFuncXformer :: MethodMemberType c -> String
hsFuncXformer func@(Constructor _ _ _) = let len = length (genericFuncArgs func)
                                       in if len > 0
                                          then "xform" ++ show (len - 1)
                                          else "xformnull"
hsFuncXformer func@(NormalMethod Static _ _ _ _) =
  let len = length (genericFuncArgs func)
  in if len > 0
     then "xform" ++ show (len - 1)
     else "xformnull"
hsFuncXformer func = let len = length (genericFuncArgs func)
                     in "xform" ++ show len

-- | Get the return type of a member function
genericFuncRet :: MethodMemberType c -> CPPType c
genericFuncRet (Constructor _ _ _) = error "Error: FIXME: constructors don't have return type" --PrimType (CPTClass (ClassName cls))
genericFuncRet (Destructor _ _ _ _ ) = PrimType CPTVoid
genericFuncRet m = func_ret m

-- | Get the arguments of a member function
genericFuncArgs :: MethodMemberType c -> Args c
genericFuncArgs (Destructor _ _ _ _) = [] -- Destructors take no arguments
genericFuncArgs f = func_args f

-- | Get the converted name of a function, use alias if exists
aliasedFuncName :: Class -> MethodMemberType c -> String
aliasedFuncName c f =
  case f of
    Constructor _ _ alias         -> maybe (constructorName c) id alias
    Destructor _ _ _ alias        -> maybe destructorName id alias
    NormalMethod _ name _ _ alias -> maybe (nonvirtualName c name) id alias

-- | Get the original function name
cppFuncName :: Class -> MethodMemberType c -> String
cppFuncName c f =   case f of
    Constructor _ _ _           -> "new"
    Destructor _ _ _ _          -> destructorName
    NormalMethod Static _ _ _ _ -> cppStaticName c (MethodMember f)
    NormalMethod _ _ _ _ _      -> func_name f

-- Some helper functions
-- | Get the name of a static member
cppStaticName :: Class -> ClassMember c -> String
cppStaticName c m = class_name c ++ "::" ++ cppMemberName m

cppMemberName :: ClassMember c -> String
cppMemberName (DataMember (DataMemberType _ name _)) = name
cppMemberName (MethodMember dm) = func_name dm

constructorName :: Class -> String
constructorName c = "new" ++ (fst.hsClassName) c

nonvirtualName :: Class -> String -> String
nonvirtualName c str = (firstLower.fst.hsClassName) c ++ str

destructorName :: String
destructorName = "delete"

