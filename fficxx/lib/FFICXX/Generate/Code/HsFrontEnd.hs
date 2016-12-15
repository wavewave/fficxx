{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.HsFrontEnd
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Code.HsFrontEnd where

import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Char                               (toLower)
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Text                               (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Data.Text.Template                      hiding (render)
import           Language.Haskell.Exts.Syntax            ( Type(..), Exp(..), Decl(..)
                                                         , ClassDecl(..), InstDecl(..), ImportDecl(..)
                                                         , Pat(..), Name(..), QOp(..), Op(..)
                                                         , Asst(..), ConDecl(..), QualConDecl(..)
                                                         , DataOrNew(..), TyVarBind (..), Binds(..)
                                                         , Rhs(..), ExportSpec(..), Namespace(..)
                                                         , unit_tycon)
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc            ( noLoc )
import           System.FilePath                         ((<.>))
-- 
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.Annotate
import           FFICXX.Generate.Type.Module
import           FFICXX.Generate.Util
import           FFICXX.Generate.Util.HaskellSrcExts
--
import Debug.Trace

-----------------

mkComment :: Int -> String -> String
mkComment indent str 
  | (not.null) str = 
    let str_lines = lines str
        indentspace = replicate indent ' ' 
        commented_lines = 
          (indentspace ++ "-- | "++head str_lines) : map (\x->indentspace ++ "--   "++x) (tail str_lines)
     in unlines commented_lines 
  | otherwise = str                

mkPostComment :: String -> String
mkPostComment str 
  | (not.null) str = 
    let str_lines = lines str 
        commented_lines = 
          ("-- ^ "++head str_lines) : map (\x->"--   "++x) (tail str_lines)
     in unlines commented_lines 
  | otherwise = str                

{-
mkHsFuncRetType :: Types -> (String,[String])
mkHsFuncRetType rtyp = 
  case rtyp of 
    SelfType -> ("a",[])
    CPT (CPTClass c') _ -> (cname,[]) where cname = (fst.hsClassName) c' 
    CPT (CPTClassRef c') _ -> (cname,[]) where cname = (fst.hsClassName) c' 
    _ -> (ctypToHsTyp Nothing rtyp,[])
-}

extractArgTypes :: Args -> ([Type],[Asst]) 
extractArgTypes lst = 
  let  (args,st) = runState (mapM mkFuncArgTypeWorker lst) ([],(0 :: Int))
  in   (args,fst st)
 where addclass c = do
         (ctxts,n) <- get 
         let cname = (fst.hsClassName) c 
             iname = typeclassNameFromStr cname 
             tvar = mkTVar ('c' : show n)
             ctxt1 = ClassA (unqual iname) [tvar]
             ctxt2 = ClassA (unqual "FPtr") [tvar]
         put (ctxt1:ctxt2:ctxts,n+1)
         return tvar
       mkFuncArgTypeWorker (typ,_var) = 
         case typ of                  
           SelfType -> return (mkTVar "a")
           CT _ _   -> return $ tycon (ctypToHsTyp Nothing typ)
           CPT (CPTClass c') _    -> addclass c'
           CPT (CPTClassRef c') _ -> addclass c' 
           _ -> error ("No such c type : " ++ show typ)  


----------------
-- | will be deprecated
classprefix :: Class -> String 
classprefix c = let ps = (map typeclassName . class_parents) c
                in  if null ps 
                    then "" 
                    else "(" ++ intercalate "," (map (++ " a") ps) ++ ") => "


-- |
hsClassDeclHeaderTmpl :: Text
hsClassDeclHeaderTmpl = "$classann\nclass ${constraint}${classname} a where"

genHsFrontDecl :: Class -> Reader AnnotateMap Decl
genHsFrontDecl c = do
  -- for the time being, let's ignore annotation.
  -- amap <- ask  
  -- let cann = maybe "" id $ M.lookup (PkgClass,class_name c) amap 
  let 
      cdecl = mkClass (classConstraints c) (typeclassName c) [mkTBind "a"] body
      sigdecl f = mkFunSig (hsFuncName c f) (functionSignature c f)
      body = map (ClsDecl . sigdecl) . virtualFuncs . class_funcs $ c 
  return cdecl

-------------------

genHsFrontInst :: Class -> Class -> [Decl]
genHsFrontInst parent child  
  | (not.isAbstractClass) child = 
    let idecl = mkInstance [] (typeclassName parent) [convertCpp2HS (Just child) SelfType] body
        defn f = mkBind1 (hsFuncName child f) [] rhs Nothing 
          where rhs = App (mkVar (hsFuncXformer f)) (mkVar (hscFuncName child f))
        body = map (InsDecl . defn) . virtualFuncs . class_funcs $ parent
    in [idecl]
  | otherwise = []
        

      
---------------------

genHsFrontInstExistCommon :: Class -> Decl 
genHsFrontInstExistCommon c = mkInstance [] "FPtr" [existtype] body
  where (highname,rawname) = hsClassName c
        hightype = tycon highname
        rawtype = tycon rawname
        existtype = TyApp (tycon "Exist") hightype
        ename = existConstructorName c
        body = [ InsType noLoc (TyApp (tycon "Raw") existtype) rawtype
               , InsDecl (mkBind1 "get_fptr" [PApp (unqual ename) [PVar (Ident "obj")] ]
                            ((mkVar "castForeignPtr") `App` ((mkVar "get_fptr") `App` (mkVar "obj")))
                            Nothing)
               , InsDecl (mkBind1 "cast_fptr_to_obj" [PVar (Ident "fptr")]
                            (App (mkVar ename)
                              (ExpTypeSig noLoc
                                (App (mkVar "cast_fptr_to_obj")
                                  (ExpTypeSig noLoc (mkVar "fptr") (TyApp (tycon "ForeignPtr") rawtype))
                                )
                                hightype
                              )
                            )
                            Nothing)
               ]



-------------------


genHsFrontInstExistVirtual :: Class -> Class -> Decl
genHsFrontInstExistVirtual p c = mkInstance [] iparent [existtype] body
  where body = map (genHsFrontInstExistVirtualMethod p c) . virtualFuncs.class_funcs $ p
        iparent = typeclassName p
        existtype = TyApp (tycon "Exist") (tycon ((fst.hsClassName) c))

genHsFrontInstExistVirtualMethod :: Class -> Class -> Function -> InstDecl
genHsFrontInstExistVirtualMethod p c f =
    case f of
      Constructor _  _ -> error "error in genHsFrontInstExistVirtualMethod"  
      Destructor _ -> InsDecl (mkBind1 fname [existx] ((mkVar fname) `App` (mkVar "x")) Nothing)
      _ -> case func_ret f of
             SelfType -> InsDecl (mkBind1 fname (existx:map mkPVar args) rhs Nothing)
             _ -> InsDecl (mkBind1 fname [existx] ((mkVar fname) `App` (mkVar "x")) Nothing)
  where fname = hsFuncName p f
        ename = existConstructorName c
        existx = PApp (unqual ename) [PVar (Ident "x")]
        rhs = (mkVar "return" `dot` mkVar ename) `App`
              mkVar "=<<" `App`
              (mkVar fname `App` foldl1 App (mkVar "x":map mkVar args))
        args  = take (length (func_args f)) . map (\x -> 'a':(show x)) $ [1..] 

---------------------

genHsFrontInstNew :: Class         -- ^ only concrete class 
                  -> Reader AnnotateMap [Decl]
genHsFrontInstNew c = do 
  -- amap <- ask
  let fs = filter isNewFunc (class_funcs c)
  return . flip concatMap fs $ \f ->
    let
        -- for the time being, let's ignore annotation.
        -- cann = maybe "" id $ M.lookup (PkgMethod, constructorName c) amap
        -- newfuncann = mkComment 0 cann
        rhs = App (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
    in mkFun (constructorName c) (functionSignature c f) [] rhs Nothing

genHsFrontInstNonVirtual :: Class -> [Decl]
genHsFrontInstNonVirtual c =
  flip concatMap nonvirtualFuncs $ \f -> 
    let rhs = App (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
    in mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing
 where nonvirtualFuncs = nonVirtualNotNewFuncs (class_funcs c)

-----

genHsFrontInstStatic :: Class -> [Decl]
genHsFrontInstStatic c =
  flip concatMap (staticFuncs (class_funcs c)) $ \f ->
    let rhs = App (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
    in mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing

-----

castBody = [ InsDecl (mkBind1 "cast" []
                       (mkVar "unsafeForeignPtrToPtr" `dot`
                        mkVar "castForeignPtr" `dot`
                        mkVar "get_fptr")
                       Nothing)
           , InsDecl (mkBind1 "uncast" []
                       (mkVar "cast_fptr_to_obj" `dot`
                        mkVar "castForeignPtr" `dot`
                        mkVar "unsafePerformIO" `dot`
                        mkVar "newForeignPtr_")
                       Nothing)
           ]

genHsFrontInstCastable :: Class -> Maybe Decl
genHsFrontInstCastable c 
  | (not.isAbstractClass) c = 
    let iname = typeclassName c
        (_,rname) = hsClassName c
        a = mkTVar "a"
        ctxt = [ ClassA (unqual iname) [a], ClassA (unqual "FPtr") [a] ]
    in Just (mkInstance ctxt "Castable" [a,TyApp tyPtr (tycon rname)] castBody)
  | otherwise = Nothing

genHsFrontInstCastableSelf :: Class -> Maybe Decl
genHsFrontInstCastableSelf c 
  | (not.isAbstractClass) c = 
    let (cname,rname) = hsClassName c
    in Just (mkInstance [] "Castable" [tycon cname, TyApp tyPtr (tycon rname)] castBody)
  | otherwise = Nothing


--------------------------

hsClassRawType :: Class -> [Decl]
hsClassRawType c =
  [ mkData    rawname [] [] []
  , mkNewtype highname []
      [ QualConDecl noLoc [] []
          (conDecl highname [TyApp tyForeignPtr rawtype])
      ]
      derivs
  , mkInstance [] "FPtr" [hightype] [ InsType noLoc (TyApp (tycon "Raw") hightype) rawtype ]
  ]
 where (highname,rawname) = hsClassName c
       hightype = tycon highname
       rawtype = tycon rawname
       derivs = [(unqual "Eq",[]),(unqual "Ord",[]),(unqual "Show",[])]

hsClassExistType :: Class -> Decl
hsClassExistType c = mkInstance [] "Existable" [hightype]
                       [ InsData noLoc DataType (TyApp (tycon "Exist") hightype)
                           [ QualConDecl noLoc [a_bind]
                               [ClassA (unqual "FPtr") [a_tvar], ClassA (unqual iname) [a_tvar] ]
                               (conDecl ename [a_tvar])
                           ]
                           []
                       ]
  where (highname,_) = hsClassName c
        hightype = tycon highname
        a_bind = UnkindedVar (Ident "a")
        a_tvar = mkTVar "a"
        iname = typeclassName c 
        ename = existConstructorName c



------------
-- upcast --
------------

genHsFrontUpcastClass :: Class -> [Decl]
genHsFrontUpcastClass c = mkFun ("upcast"++highname) typ [mkPVar "h"] rhs Nothing
  where (highname,rawname) = hsClassName c
        hightype = tycon highname
        rawtype = tycon rawname
        iname = typeclassName c
        a_bind = UnkindedVar (Ident "a")
        a_tvar = mkTVar "a"
        typ = TyForall (Just [a_bind])
                [ClassA (unqual "FPtr") [a_tvar], ClassA (unqual iname) [a_tvar]]
                (TyFun a_tvar hightype)
        rhs = Let (BDecls
                    [ pbind (mkPVar "fh") (UnGuardedRhs (App (mkVar "get_fptr") (mkVar "h"))) Nothing
                    , pbind (mkPVarSig "fh2" (TyApp tyForeignPtr rawtype))
                        (UnGuardedRhs (App (mkVar "castForeignPtr") (mkVar "fh"))) Nothing
                    ] 
                  ) 
                  (mkVar "cast_fptr_to_obj" `App` mkVar "fh2")


--------------
-- downcast --
--------------

genHsFrontDowncastClass :: Class -> [Decl]
genHsFrontDowncastClass c = mkFun ("downcast"++highname) typ [mkPVar "h"] rhs Nothing
  where (highname,rawname) = hsClassName c
        hightype = tycon highname
        rawtype = tycon rawname
        iname = typeclassName c
        a_bind = UnkindedVar (Ident "a")
        a_tvar = mkTVar "a"
        typ = TyForall (Just [a_bind])
                [ClassA (unqual "FPtr") [a_tvar], ClassA (unqual iname) [a_tvar]]
                (TyFun hightype a_tvar)
        rhs = Let (BDecls
                    [ pbind (mkPVar "fh") (UnGuardedRhs (App (mkVar "get_fptr") (mkVar "h"))) Nothing
                    , pbind (mkPVar "fh2") (UnGuardedRhs (App (mkVar "castForeignPtr") (mkVar "fh"))) Nothing
                    ] 
                  ) 
                  (mkVar "cast_fptr_to_obj" `App` mkVar "fh2")


------------------------
-- Top Level Function --
------------------------

genTopLevelFuncDef :: TopLevelFunction -> [Decl]
genTopLevelFuncDef f@TopLevelFunction {..} = 
    let fname = hsFrontNameForTopLevelFunction f
        (atyps,ctxts) = extractArgTypes toplevelfunc_args
        rtyp = (tycon . ctypToHsTyp Nothing) toplevelfunc_ret
        sig = TyForall Nothing ctxts (foldr1 TyFun (atyps ++ [TyApp (tycon "IO") rtyp]))
        xformerstr = let len = length toplevelfunc_args in if len > 0 then "xform" ++ show (len-1) else "xformnull"
        cfname = "c_" ++ toLowers fname 
        rhs = App (mkVar xformerstr) (mkVar cfname)
        
    in mkFun fname sig [] rhs Nothing 
genTopLevelFuncDef v@TopLevelVariable {..} = 
    let fname = hsFrontNameForTopLevelFunction v
        cfname = "c_" ++ toLowers fname 
        rtyp = (tycon . ctypToHsTyp Nothing) toplevelvar_ret
        sig = TyApp (tycon "IO") rtyp
        rhs = App (mkVar "xformnull") (mkVar cfname)
        
    in mkFun fname sig [] rhs Nothing 


------------
-- Export --
------------

genExport :: Class -> [ExportSpec]
genExport c =
    let espec n = if null . (filter isVirtualFunc) $ (class_funcs c) 
                    then EAbs NoNamespace (unqual n)
                    else EThingAll (unqual n)
    in if isAbstractClass c 
       then [ espec (typeclassName c) ]
       else [ EThingAll (unqual ((fst.hsClassName) c))
            , espec (typeclassName c)
            , EVar (unqual ("upcast" ++ (fst.hsClassName) c))
            , EVar (unqual ("downcast" ++ (fst.hsClassName) c)) ]
            ++ genExportConstructorAndNonvirtual c 
            ++ genExportStatic c 

-- | constructor and non-virtual function 
genExportConstructorAndNonvirtual :: Class -> [ExportSpec]
genExportConstructorAndNonvirtual c = map (EVar . unqual) fns
  where fs = class_funcs c
        fns = map (aliasedFuncName c) (constructorFuncs fs 
                                       ++ nonVirtualNotNewFuncs fs)

-- | staic function export list 
genExportStatic :: Class -> [ExportSpec]
genExportStatic c = map (EVar . unqual) fns
  where fs = class_funcs c
        fns = map (aliasedFuncName c) (staticFuncs fs) 

genImportInModule :: [Class] -> [ImportDecl]
genImportInModule = concatMap (\x -> map (\y -> mkImport (getClassModuleBase x<.>y)) ["RawType","Interface","Implementation"])

genImportInFFI :: ClassModule -> [ImportDecl]
genImportInFFI = map (\x->mkImport (x <.> "RawType")) . cmImportedModulesForFFI


genImportInInterface :: ClassModule -> [ImportDecl]
genImportInInterface m = 
  let modlstraw = cmImportedModulesRaw m
      modlstparent = cmImportedModulesHighNonSource m 
      modlsthigh = cmImportedModulesHighSource m
  in  [mkImport (cmModule m <.> "RawType")]
      ++ map (\x -> mkImport (x<.>"RawType")) modlstraw
      ++ map (\x -> mkImport (x<.>"Interface")) modlstparent 
      ++ map (\x -> mkImportSrc (x<.>"Interface")) modlsthigh

-- |
genImportInCast :: ClassModule -> [ImportDecl]
genImportInCast m = [ mkImport (cmModule m <.> "RawType")
                   ,  mkImport (cmModule m <.> "Interface") ]

-- | 
genImportInImplementation :: ClassModule -> [ImportDecl]
genImportInImplementation m = 
  let modlstraw' = cmImportedModulesForFFI m
      modlsthigh = nub $ map getClassModuleBase $ concatMap class_allparents (cmClass m)
      modlstraw = filter (not.(flip elem modlsthigh)) modlstraw' 
  in  [ mkImport (cmModule m <.> "RawType")
      , mkImport (cmModule m <.> "FFI")
      , mkImport (cmModule m <.> "Interface")
      , mkImport (cmModule m <.> "Cast") ]
      ++ concatMap (\x -> map (\y -> mkImport (x<.>y)) ["RawType","Cast","Interface"]) modlstraw
      ++ concatMap (\x -> map (\y -> mkImport (x<.>y)) ["RawType","Cast","Interface"]) modlsthigh

{- 
-- | 
genImportInExistential :: DaughterMap -> ClassModule -> String
genImportInExistential dmap m = 
  let daughters = concat . catMaybes $ (map (flip M.lookup dmap . getClassModuleBase)  (cmClass m))
      alldaughters = nub . map getClassModuleBase $ daughters
      getImportOneClass mname = 
          intercalateWith connRet (importOneClass mname) 
                          ["RawType", "Cast", "Interface", "Implementation"]
  in  intercalateWith connRet getImportOneClass alldaughters
-}


genTmplDecl :: TemplateClass -> [Decl]
genTmplDecl t = [ mkData rname [mkTBind "a"] [] []
                , mkNewtype hname [mkTBind "a"]
                    [ QualConDecl noLoc [] [] (conDecl hname [TyApp tyForeignPtr rawtype]) ] []
                , mkClass [] (typeclassNameT t) [mkTBind "a"] methods
                    
                ]
  where (hname,rname) = hsTemplateClassName t
        hightype = tycon hname
        rawtype = TyApp (tycon rname) (mkTVar "a")
        sigdecl f = mkFunSig (tfun_name f) (functionSignature t f)
        methods = map (ClsDecl . sigdecl) (tclass_funcs t)
