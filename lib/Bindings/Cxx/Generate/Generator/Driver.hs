module Bindings.Cxx.Generate.Generator.Driver where

import System.Directory 
import System.FilePath
import System.IO
import System.Process
import Text.StringTemplate
-- import Text.StringTemplate.Helpers
--
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Annotate
import Bindings.Cxx.Generate.Generator.ContentMaker 
import Bindings.Cxx.Generate.Util



----
---- Header and Cpp file
----

writeTypeDeclHeaders :: STGroup String -> ClassGlobal 
                     -> FilePath -> String -> [ClassImportHeader]
                     -> IO ()
writeTypeDeclHeaders templates cglobal wdir cprefix headers = do 
  let fn = wdir </> cprefix ++ "Type.h"
      classes = map cihClass headers
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkTypeDeclHeader templates cglobal classes)

writeDeclHeaders :: STGroup String -> ClassGlobal 
                 -> FilePath -> String -> ClassImportHeader
                 -> IO () 
writeDeclHeaders templates cglobal wdir cprefix header = do 
  let fn = wdir </> cihSelfHeader header
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkDeclHeader templates cglobal cprefix header)

writeCppDef :: STGroup String -> FilePath -> ClassImportHeader -> IO () 
writeCppDef templates wdir header = do 
  let fn = wdir </> cihSelfCpp header
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkDefMain templates header)

-- | 
writeRawTypeHs :: STGroup String -> FilePath -> ClassModule -> IO ()
writeRawTypeHs templates wdir mod = do
  let fn = wdir </> cmModule mod <.> rawtypeHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkRawTypeHs templates mod) 

-- | 
writeFFIHsc :: STGroup String -> FilePath -> ClassModule -> IO ()
writeFFIHsc templates wdir mod = do 
  let fn = wdir </> cmModule mod <.> ffiHscFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkFFIHsc templates mod)

-- | 
writeInterfaceHs :: AnnotateMap -> STGroup String -> FilePath 
                 -> ClassModule 
                 -> IO ()
writeInterfaceHs amap templates wdir mod = do 
  let fn = wdir </> cmModule mod <.> interfaceHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkInterfaceHs amap templates mod)

-- |
writeCastHs :: STGroup String -> FilePath -> ClassModule 
            -> IO ()
writeCastHs templates wdir mod = do 
  let fn = wdir </> cmModule mod <.> castHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkCastHs templates mod)

-- | 
writeImplementationHs :: AnnotateMap 
                      -> STGroup String 
                      -> FilePath 
                      -> ClassModule 
                      -> IO ()
writeImplementationHs amap templates wdir mod = do 
  let fn = wdir </> cmModule mod <.> implementationHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkImplementationHs amap templates mod)

-- | 
writeExistentialHs :: STGroup String 
                   -> ClassGlobal 
                   -> FilePath 
                   -> ClassModule 
                   -> IO ()
writeExistentialHs templates cglobal wdir mod = do 
  let fn = wdir </> cmModule mod <.> existentialHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkExistentialHs templates cglobal mod)

-- |
writeModuleHs :: STGroup String -> FilePath -> ClassModule -> IO () 
writeModuleHs templates wdir mod = do 
  let fn = wdir </> cmModule mod <.> "hs"
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkModuleHs templates mod)

writePkgHs :: String -- ^ package name
           -> STGroup String 
           -> FilePath 
           -> [ClassModule] 
           -> IO () 
writePkgHs pkgname templates wdir mods = do 
  let fn = wdir </> pkgname <.> "hs"
      exportListStr = intercalateWith conncomma ((\x->" module " ++ x).cmModule) mods 
      importListStr = intercalateWith connRet ((\x->"import " ++ x).cmModule) mods
      str = renderTemplateGroup 
              templates 
              [ ("exportList", exportListStr) 
              , ("importList", importListStr) ]
              "Pkg.hs"
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h str


notExistThenCreate :: FilePath -> IO () 
notExistThenCreate dir = do 
    b <- doesDirectoryExist dir
    if b then return () else system ("mkdir -p " ++ dir) >> return ()


-- | now only create directory
copyPredefined :: FilePath -> FilePath -> String -> IO () 
copyPredefined tdir ddir prefix = do 
    return () 
    -- notExistThenCreate (ddir </> prefix)
    -- copyFile (tdir </> "TypeCast.hs" ) (ddir </> prefix </> "TypeCast.hs") 


copyCppFiles :: FilePath -> FilePath -> String -> ClassImportHeader -> IO ()
copyCppFiles wdir ddir cprefix header = do 
  let thfile = cprefix ++ "Type.h"
      hfile = cihSelfHeader header
      cppfile = cihSelfCpp header
  copyFile (wdir </> thfile) (ddir </> thfile) 
  copyFile (wdir </> hfile) (ddir </> hfile) 
  copyFile (wdir </> cppfile) (ddir </> cppfile)

copyModule :: FilePath -> FilePath -> String -> ClassModule -> IO ()
copyModule wdir ddir pkgname mod = do 
  let modbase = cmModule mod 
  let onefilecopy fname = do 
        let (fnamebody,fnameext) = splitExtension fname
            (mdir,mfile) = moduleDirFile fnamebody
            origfpath = wdir </> fname
            (mfile',mext') = splitExtension mfile
            newfpath = ddir </> mdir </> mfile' ++ fnameext   
        -- b <- doesDirectoryExist (ddir</>mdir)
        -- if b then return () else createDirectory (ddir</>mdir) 
        -- print (ddir,mdir)
        notExistThenCreate (ddir </> mdir) 
        copyFile origfpath newfpath 

  onefilecopy $ modbase ++ ".hs"
  onefilecopy $ modbase ++ ".RawType.hs"
  onefilecopy $ modbase ++ ".FFI.hsc"
  onefilecopy $ modbase ++ ".Interface.hs"
  onefilecopy $ modbase ++ ".Cast.hs"
  onefilecopy $ modbase ++ ".Implementation.hs"
  -- onefilecopy $ prefix <.> modbase ++ ".Existential.hs"

  copyFile (wdir </> pkgname <.> "hs") (ddir </> pkgname <.> "hs")

  return ()
