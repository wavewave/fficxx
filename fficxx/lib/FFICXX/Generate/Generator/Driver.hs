-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Generator.Driver
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
-- 
-- License     : BSD3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Generator.Driver where

import           Control.Applicative ((<$>))
import           Control.Monad (when, forM_)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Digest.Pure.MD5
import           System.Directory 
import           System.FilePath
import           System.IO
import           System.Process
import           Text.StringTemplate
--
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Annotate
import FFICXX.Generate.Type.PackageInterface 
import FFICXX.Generate.Generator.ContentMaker 
import FFICXX.Generate.Util

----
---- Header and Cpp file
----

-- | 
writeTypeDeclHeaders :: FilePath 
                     -> TypeMacro  -- ^ type macro 
                     -> String  -- ^ cprefix 
                     -> [ClassImportHeader]
                     -> IO ()
writeTypeDeclHeaders wdir typemacro cprefix headers =
  let fn = wdir </> cprefix ++ "Type.h"
      classes = map cihClass headers
  in withFile fn WriteMode $ \h ->
       hPutStrLn h (mkTypeDeclHeader typemacro classes)

-- | 
writeDeclHeaders :: FilePath 
                 -> TypeMacro 
                 -> String  -- ^ c prefix 
                 -> ClassImportHeader
                 -> IO () 
writeDeclHeaders wdir typemacroprefix cprefix header =
  let fn = wdir </> (unHdrName (cihSelfHeader header))
  in withFile fn WriteMode $ \h -> 
       hPutStrLn h (mkDeclHeader typemacroprefix cprefix header)

-- | 
writeTopLevelFunctionHeaders :: FilePath 
                             -> TypeMacro 
                             -> String  -- ^ c prefix 
                             -> TopLevelImportHeader
                             -> IO () 
writeTopLevelFunctionHeaders wdir typemacroprefix cprefix tih =
  let fn = wdir </> tihHeaderFileName tih <.> "h"
  in withFile fn WriteMode $ \h -> 
       hPutStrLn h (mkTopLevelFunctionHeader typemacroprefix cprefix tih)

writeCppDef :: FilePath -> ClassImportHeader -> IO () 
writeCppDef wdir header = 
  let fn = wdir </> cihSelfCpp header
  in withFile fn WriteMode $ \h -> hPutStrLn h (mkDefMain header)


-- | 
writeTopLevelFunctionCppDef :: FilePath 
                            -> TypeMacro 
                            -> String  -- ^ c prefix 
                            -> TopLevelImportHeader
                            -> IO () 
writeTopLevelFunctionCppDef wdir typemacroprefix cprefix tih = 
  let fn = wdir </> tihHeaderFileName tih <.> "cpp"
  in withFile fn WriteMode $ \h -> hPutStrLn h (mkTopLevelFunctionCppDef cprefix tih)


-- | 
writeRawTypeHs :: FilePath -> ClassModule -> IO ()
writeRawTypeHs wdir m =
  let fn = wdir </> cmModule m <.> "RawType" <.> "hs"
  in withFile fn WriteMode $ \h -> hPutStrLn h (mkRawTypeHs m) 

-- | 
writeFFIHsc :: FilePath -> ClassModule -> IO ()
writeFFIHsc wdir m = 
  let fn = wdir </> cmModule m <.> "FFI" <.> "hsc"
  in withFile fn WriteMode $ \h -> hPutStrLn h (mkFFIHsc m)

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
writeInterfaceHSBOOT :: FilePath -> String -> IO ()
writeInterfaceHSBOOT wdir mname =
  let fn = wdir </> mname <.> "Interface" <.> "hs-boot"
  in withFile fn WriteMode $ \h -> hPutStrLn h (mkInterfaceHSBOOT mname)

-- |
writeModuleHs :: FilePath -> ClassModule -> IO () 
writeModuleHs wdir m =
  let fn = wdir </> cmModule m <.> "hs"
  in withFile fn WriteMode $ \h -> do 
       hPutStrLn h (mkModuleHs m)

-- | 
writePkgHs :: String -- ^ summary module 
           -> FilePath 
           -> [ClassModule] 
           -> TopLevelImportHeader
           -> IO () 
writePkgHs modname wdir mods tih = 
  let fn = wdir </> modname <.> "hs"
      str = mkPkgHs modname mods tih
  in withFile fn WriteMode $ \h -> hPutStrLn h str


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


copyCppFiles :: FilePath -> FilePath -> String -> (TopLevelImportHeader,[ClassImportHeader]) -> IO ()
copyCppFiles wdir ddir cprefix (tih,cihs) = do 
  let thfile = cprefix ++ "Type.h"
      tlhfile = tihHeaderFileName tih <.> "h"
      tlcppfile = tihHeaderFileName tih <.> "cpp"
  copyFileWithMD5Check (wdir </> thfile) (ddir </> thfile) 
  doesFileExist (wdir </> tlhfile) 
    >>= flip when (copyFileWithMD5Check (wdir </> tlhfile) (ddir </> tlhfile))
  doesFileExist (wdir </> tlcppfile) 
    >>= flip when (copyFileWithMD5Check (wdir </> tlcppfile) (ddir </> tlcppfile))
  forM_ cihs $ \header-> do 
    let hfile = unHdrName (cihSelfHeader header)
        cppfile = cihSelfCpp header
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
