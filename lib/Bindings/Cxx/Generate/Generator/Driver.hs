-----------------------------------------------------------------------------
-- |
-- Module      : Bindings.Cxx.Generate.Generator.Driver
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- 
--
-----------------------------------------------------------------------------

module Bindings.Cxx.Generate.Generator.Driver where

import           Control.Applicative ((<$>))
import           Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Digest.Pure.MD5
import           System.Directory 
import           System.FilePath
import           System.IO
import           System.Process
import           Text.StringTemplate
-- import Text.StringTemplate.Helpers
--
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Annotate
import Bindings.Cxx.Generate.Type.PackageInterface 
import Bindings.Cxx.Generate.Generator.ContentMaker 
import Bindings.Cxx.Generate.Util



----
---- Header and Cpp file
----

-- | 
writeTypeDeclHeaders :: STGroup String 
                     -> FilePath 
                     -> TypeMacro  -- ^ type macro 
                     -> String  -- ^ cprefix 
                     -> [ClassImportHeader]
                     -> IO ()
writeTypeDeclHeaders templates wdir typemacro cprefix headers = do 
  let fn = wdir </> cprefix ++ "Type.h"
      classes = map cihClass headers
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkTypeDeclHeader templates typemacro classes)

-- | 
writeDeclHeaders :: STGroup String  
                 -> FilePath 
                 -> TypeMacro 
                 -> String  -- ^ c prefix 
                 -> ClassImportHeader
                 -> IO () 
writeDeclHeaders templates wdir typemacroprefix cprefix header = do 
  let fn = wdir </> cihSelfHeader header
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkDeclHeader templates typemacroprefix cprefix header)

writeCppDef :: STGroup String -> FilePath -> ClassImportHeader -> IO () 
writeCppDef templates wdir header = do 
  let fn = wdir </> cihSelfCpp header
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkDefMain templates header)

-- | 
writeRawTypeHs :: STGroup String -> FilePath -> ClassModule -> IO ()
writeRawTypeHs templates wdir m = do
  let fn = wdir </> cmModule m <.> rawtypeHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkRawTypeHs templates m) 

-- | 
writeFFIHsc :: STGroup String -> FilePath -> ClassModule -> IO ()
writeFFIHsc templates wdir m = do 
  let fn = wdir </> cmModule m <.> ffiHscFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkFFIHsc templates m)

-- | 
writeInterfaceHs :: AnnotateMap -> STGroup String -> FilePath 
                 -> ClassModule 
                 -> IO ()
writeInterfaceHs amap templates wdir m = do 
  let fn = wdir </> cmModule m <.> interfaceHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkInterfaceHs amap templates m)

-- |
writeCastHs :: STGroup String -> FilePath -> ClassModule -> IO ()
writeCastHs templates wdir m = do 
  let fn = wdir </> cmModule m <.> castHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkCastHs templates m)

-- | 
writeImplementationHs :: AnnotateMap 
                      -> STGroup String 
                      -> FilePath 
                      -> ClassModule 
                      -> IO ()
writeImplementationHs amap templates wdir m = do 
  let fn = wdir </> cmModule m <.> implementationHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkImplementationHs amap templates m)

-- | 
writeExistentialHs :: STGroup String 
                   -> ClassGlobal 
                   -> FilePath 
                   -> ClassModule 
                   -> IO ()
writeExistentialHs templates cglobal wdir m = do 
  let fn = wdir </> cmModule m <.> existentialHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkExistentialHs templates cglobal m)

-- | 
writeInterfaceHSBOOT :: STGroup String -> FilePath -> String -> IO ()
writeInterfaceHSBOOT templates wdir mname = do 
  let fn = wdir </> mname <.> "Interface" <.> "hs-boot"
  withFile fn WriteMode $ \h -> hPutStrLn h (mkInterfaceHSBOOT templates mname)

-- |
writeModuleHs :: STGroup String -> FilePath -> ClassModule -> IO () 
writeModuleHs templates wdir m = do 
  let fn = wdir </> cmModule m <.> "hs"
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkModuleHs templates m)

writePkgHs :: String -- ^ summary module 
           -> STGroup String 
           -> FilePath 
           -> [ClassModule] 
           -> IO () 
writePkgHs modname templates wdir mods = do 
  let fn = wdir </> modname <.> "hs"
      exportListStr = intercalateWith (conn "\n, ") ((\x->"module " ++ x).cmModule) mods 
      importListStr = intercalateWith connRet ((\x->"import " ++ x).cmModule) mods
      str = renderTemplateGroup 
              templates 
              [ ("summarymod", modname)
              , ("exportList", exportListStr) 
              , ("importList", importListStr) ]
              "Pkg.hs"
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h str


notExistThenCreate :: FilePath -> IO () 
notExistThenCreate dir = do 
    b <- doesDirectoryExist dir
    if b then return () else system ("mkdir -p " ++ dir) >> return ()


{- 
-- | now only create directory
copyPredefined :: FilePath -> FilePath -> String -> IO () 
copyPredefined _tdir _ddir _prefix = do 
    return () 
    -- notExistThenCreate (ddir </> prefix)
    -- copyFile (tdir </> "TypeCast.hs" ) (ddir </> prefix </> "TypeCast.hs") 
-}

copyFileWithMD5Check :: FilePath -> FilePath -> IO () 
copyFileWithMD5Check src tgt = do
  b <- doesFileExist tgt 
  if b 
    then do 
      srcmd5 <- md5 <$> L.readFile src  
      tgtmd5 <- md5 <$> L.readFile tgt 
      if srcmd5 == tgtmd5 then return () else copyFile src tgt 
    else copyFile src tgt  


copyCppFiles :: FilePath -> FilePath -> String -> ClassImportHeader -> IO ()
copyCppFiles wdir ddir cprefix header = do 
  let thfile = cprefix ++ "Type.h"
      hfile = cihSelfHeader header
      cppfile = cihSelfCpp header
  copyFileWithMD5Check (wdir </> thfile) (ddir </> thfile) 
  copyFileWithMD5Check (wdir </> hfile) (ddir </> hfile) 
  copyFileWithMD5Check (wdir </> cppfile) (ddir </> cppfile)

copyModule :: FilePath -> FilePath -> String -> ClassModule -> IO ()
copyModule wdir ddir summarymod m = do 
  let modbase = cmModule m 
  let onefilecopy fname = do 
        let (fnamebody,fnameext) = splitExtension fname
            (mdir,mfile) = moduleDirFile fnamebody
            origfpath = wdir </> fname
            (mfile',_mext') = splitExtension mfile
            newfpath = ddir </> mdir </> mfile' ++ fnameext   
        b <- doesFileExist origfpath 
        when b $ do 
          notExistThenCreate (ddir </> mdir) 
          copyFileWithMD5Check origfpath newfpath 

  onefilecopy $ modbase ++ ".hs"
  onefilecopy $ modbase ++ ".RawType.hs"
  onefilecopy $ modbase ++ ".FFI.hsc"
  onefilecopy $ modbase ++ ".Interface.hs"
  onefilecopy $ modbase ++ ".Cast.hs"
  onefilecopy $ modbase ++ ".Implementation.hs"
  onefilecopy $ modbase ++ ".Interface.hs-boot"
  onefilecopy $ summarymod <.> "hs"
  return ()
