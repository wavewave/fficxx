module HROOT.Generate.Code.HsFrontEnd where

import qualified Data.Map as M

import HROOT.Generate.Type.CType
import HROOT.Generate.Type.Method
import HROOT.Generate.Type.Class

import HROOT.Generate.Util

import Data.List
import Data.Maybe

import Control.Monad.State
----------------

classprefix :: Class -> String 
classprefix c = let ps = (map typeclassName . class_parents) c
                in  if null ps 
                    then "" 
                    else "(" ++ intercalate "," (map (++ " a") ps) ++ ") => "

hsClassDeclHeaderTmpl :: String
hsClassDeclHeaderTmpl = "class $constraint$$classname$ a where"


genHsFrontDecl :: Class -> String 
genHsFrontDecl c =  
  let header = render hsClassDeclHeaderTmpl [ ("classname", typeclassName c ) 
                                            , ("constraint", classprefix c) ] 
      bodyline func = render hsClassDeclFuncTmpl 
                                    [ ("funcname", hsFuncName c func) 
                                    , ("args" , prefixstr func ++ argstr func ) 
                                    ] 
      prefixstr func =  
        let prefixlst = (snd . mkHsFuncArgType c . genericFuncArgs) func
                        ++ (snd . mkHsFuncRetType c ) func
        in  if null prefixlst
              then "" 
              else "(" ++ (intercalateWith conncomma id prefixlst) ++ ") => "  
                  
      argstr func = intercalateWith connArrow id $
                      [ "a" ] 
                      ++ fst (mkHsFuncArgType c (genericFuncArgs func))
                      ++ ["IO " ++ fst (mkHsFuncRetType c func)]  
      bodylines = map bodyline . virtualFuncs 
                      $ (class_funcs c) 
  in  intercalateWith connRet id (header : bodylines) 



genAllHsFrontDecl :: [Class] -> String 
genAllHsFrontDecl = intercalateWith connRet2 genHsFrontDecl

-------------------



genHsFrontInst :: Class -> Class -> String 
genHsFrontInst parent child  
  | (not.isAbstractClass) child = 
    let headline = "instance " ++ typeclassName parent ++ " " ++ class_name child ++ " where" 
        defline func = "  " ++ hsFuncName child func ++ " = " ++ hsFuncXformer func ++ " " ++ hscFuncName child func 
        deflines = (map defline) . virtualFuncs . class_funcs $ parent 

    in  intercalateWith connRet id (headline : deflines) 
  | otherwise = ""
        

   
genAllHsFrontInst :: [Class] -> DaughterMap -> String 
genAllHsFrontInst cs m = 
  let selflst = map (\x->(x,[x])) cs 
      lst = selflst ++ M.toList m  
      f (x,ys) = intercalateWith connRet2 (genHsFrontInst x) ys
  in intercalateWith connRet2 f lst
      
---------------------
hsClassInstExistTmpl :: String 
hsClassInstExistTmpl = "instance FPtr (Exist $highname$) where\n  type Raw (Exist $highname$) = $rawname$\n  get_fptr ($existConstructor$ obj) = castForeignPtr (get_fptr obj)\n  cast_fptr_to_obj fptr = $existConstructor$ (cast_fptr_to_obj (fptr :: ForeignPtr $rawname$) :: $highname$)\n\ninstance Castable (Exist $highname$) (Ptr $rawname$) where\n  cast = unsafeForeignPtrToPtr . get_fptr\n  uncast = cast_fptr_to_obj . unsafePerformIO . newForeignPtr_" 

genHsFrontInstExist :: Class -> String 
genHsFrontInstExist c = render hsClassInstExistTmpl tmplName
  where (highname,rawname) = hsClassName c
        iname = typeclassName c 
        ename = existConstructorName c
        tmplName = [ ("rawname",rawname)
                   , ("highname",highname)
                   , ("interfacename",iname)
                   , ("existConstructor",ename)
                   ] 

genAllHsFrontInstExist :: [Class] -> String 
genAllHsFrontInstExist cs = intercalateWith connRet2 genHsFrontInstExist cs

 
---------------------

genHsFrontInstNew :: Class         -- ^ only concrete class 
                    -> Maybe String 
genHsFrontInstNew c = 
  if null newfuncs 
  then Nothing
  else let newfunc = head newfuncs
           newlinehead = "new" ++ class_name c ++ " :: " ++ argstr newfunc 
           newlinebody = "new" ++ class_name c ++ " = " ++ hsFuncXformerNew newfunc ++ " " ++ hscFuncName c newfunc 
           argstr func = intercalateWith connArrow id $
                           map (ctypeToHsType c.fst) (genericFuncArgs func)
                           ++ ["IO " ++ (ctypeToHsType c.genericFuncRet) func]
           newline = newlinehead ++ "\n" ++ newlinebody 
       in Just newline
  where newfuncs = filter isNewFunc (class_funcs c)  

genAllHsFrontInstNew :: [Class]    -- ^ only concrete class 
                     -> String 
genAllHsFrontInstNew = intercalate "\n\n" . map fromJust . filter isJust . map genHsFrontInstNew 
  
genHsFrontInstNonVirtual :: Class -> Maybe String 
genHsFrontInstNonVirtual c 
  | (not.null) nonvirtualFuncs  =                        
    let header f = (aliasedFuncName c f) ++ " :: " ++ argstr f
        body f  = (aliasedFuncName c f)  ++ " = " ++ hsFuncXformer f ++ " " ++ hscFuncName c f 
        argstr func = intercalateWith connArrow id $ 
                        [class_name c]  
                        ++ map (ctypeToHsType c.fst) (genericFuncArgs func)
                        ++ ["IO " ++ (ctypeToHsType c . genericFuncRet) func] 
    in  Just $ intercalateWith connRet2 (\f -> header f ++ "\n" ++ body f) nonvirtualFuncs
  | otherwise = Nothing   
 where nonvirtualFuncs = nonVirtualNotNewFuncs (class_funcs c)

genAllHsFrontInstNonVirtual :: [Class] -> String 
genAllHsFrontInstNonVirtual = intercalate "\n\n" . map fromJust . filter isJust . map genHsFrontInstNonVirtual

genHsFrontInstCastable :: Class -> String 
genHsFrontInstCastable c 
  | (not.isAbstractClass) c = 
    let iname = typeclassName c
        (_,rname) = hsClassName c
    in  render hsInterfaceCastableInstanceTmpl 
               [("interfaceName",iname),("rawClassName",rname)]
  | otherwise = "" 

genAllHsFrontInstCastable :: [Class] -> String 
genAllHsFrontInstCastable = 
  intercalateWith connRet2 genHsFrontInstCastable



--------------------------

rawToHighDecl :: String
rawToHighDecl = "data $rawname$\nnewtype $highname$ = $highname$ (ForeignPtr $rawname$) deriving (Eq, Ord, Show)"

rawToHighInstance :: String
rawToHighInstance = "instance FPtr $highname$ where\n   type Raw $highname$ = $rawname$\n   get_fptr ($highname$ fptr) = fptr\n   cast_fptr_to_obj = $highname$"


existableInstance :: String 
existableInstance = "instance Existable $highname$ where\n  data Exist $highname$ = forall a. (FPtr a, $interfacename$ a) => $existConstructor$ a"


hsClassType :: Class -> String 
hsClassType c = let decl = render rawToHighDecl tmplName
                    inst1 = render rawToHighInstance tmplName
                    exist1 = render existableInstance tmplName
                in  decl `connRet` inst1 `connRet` exist1
  where (highname,rawname) = hsClassName c
        iname = typeclassName c 
        ename = existConstructorName c
        tmplName = [ ("rawname",rawname)
                   , ("highname",highname)
                   , ("interfacename",iname)
                   , ("existConstructor",ename)
                   ] 
            
mkRawClasses :: [Class] -> String 
mkRawClasses = intercalateWith connRet2 hsClassType



hsClassDeclFuncTmpl :: String
hsClassDeclFuncTmpl = "    $funcname$ :: $args$ "


hsArgs :: Class -> Args -> String
hsArgs c = intercalateWith connArrow (ctypeToHsType c. fst) 

mkHsFuncArgType :: Class -> Args -> ([String],[String]) 
mkHsFuncArgType c lst = 
  let  (args,st) = runState (mapM mkFuncArgTypeWorker lst) ([],(0 :: Int))
  in   (args,fst st)
  where mkFuncArgTypeWorker (typ,_var) = 
          case typ of                  
            SelfType -> return "a"
            CT _ _   -> return $ ctypeToHsType c typ 
            CPT (CPTClass cname) _ -> do 
              (prefix,n) <- get 
              let iname = typeclassNameFromStr cname -- typeclassName c
                  newname = 'c' : show n
                  newprefix1 = iname ++ " " ++ newname    
                  newprefix2 = "FPtr " ++ newname
              put (newprefix1:newprefix2:prefix,n+1)
              return newname
            _ -> error ("No such c type : " ++ show typ)  

mkHsFuncRetType :: Class -> Function -> (String,[String])
mkHsFuncRetType c func = 
  let rtyp = genericFuncRet func
  in case rtyp of 
    SelfType -> ("a",[])
    CPT (CPTClass cname) _ -> ("(Exist " ++ cname ++ ")",[])
  {-    let iname = typeclassNameFromStr cname -- typeclassName c
          newname = "b"
          newprefix1 = iname ++ " " ++ newname    
          newprefix2 = "FPtr " ++ newname
      in  ("b",[newprefix1,newprefix2]) -}
    _ -> (ctypeToHsType c rtyp,[])

      
----------                        

hsInterfaceCastableInstanceTmpl :: String 
hsInterfaceCastableInstanceTmpl = 
  "instance ($interfaceName$ a, FPtr a) => Castable a (Ptr $rawClassName$) where\n  cast = unsafeForeignPtrToPtr . castForeignPtr . get_fptr\n  uncast = cast_fptr_to_obj . castForeignPtr . unsafePerformIO . newForeignPtr_ \n"




----------
