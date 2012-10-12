module Bindings.Cxx.Generate.Generator.Driver where

import System.Directory 
import System.FilePath
import System.IO

import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Annotate
import Bindings.Cxx.Generate.Generator.ContentMaker 
import Bindings.Cxx.Generate.Util

import Text.StringTemplate
import Text.StringTemplate.Helpers

import HEP.Util.File 

-----
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

writeRawTypeHs :: STGroup String -> FilePath -> String -> ClassModule -> IO ()
writeRawTypeHs templates wdir prefix mod = do
  let fn = wdir </> prefix <.> cmModule mod <.> rawtypeHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkRawTypeHs templates prefix mod) 

writeFFIHsc :: STGroup String -> FilePath -> String -> ClassModule -> IO ()
writeFFIHsc templates wdir prefix mod = do 
  let fn = wdir </> prefix <.> cmModule mod <.> ffiHscFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkFFIHsc templates prefix mod)

writeInterfaceHs :: AnnotateMap -> STGroup String -> FilePath 
                 -> String -> ClassModule 
                 -> IO ()
writeInterfaceHs amap templates wdir prefix mod = do 
  let fn = wdir </> prefix <.> cmModule mod <.> interfaceHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkInterfaceHs amap templates prefix mod)

writeCastHs :: STGroup String -> FilePath -> String ->  ClassModule 
            -> IO ()
writeCastHs templates wdir prefix mod = do 
  let fn = wdir </> prefix <.> cmModule mod <.> castHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkCastHs templates prefix mod)

writeImplementationHs :: AnnotateMap -> STGroup String -> FilePath 
                      -> String -> ClassModule 
                      -> IO ()
writeImplementationHs amap templates wdir prefix mod = do 
  let fn = wdir </> prefix <.> cmModule mod <.> implementationHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkImplementationHs amap templates prefix mod)

writeExistentialHs :: STGroup String -> ClassGlobal -> FilePath 
                   -> String -> ClassModule 
                   -> IO ()
writeExistentialHs templates cglobal wdir prefix mod = do 
  let fn = wdir </> prefix <.> cmModule mod <.> existentialHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkExistentialHs templates cglobal prefix mod)


writeModuleHs :: STGroup String -> FilePath 
              -> String -> ClassModule -> IO () 
writeModuleHs templates wdir prefix mod = do 
  let fn = wdir </> prefix <.> cmModule mod <.> "hs"
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkModuleHs templates prefix mod)

writePkgHs :: (String,String) -- ^ (package name, module prefix) 
           -> STGroup String 
           -> FilePath 
           -> [ClassModule] 
           -> IO () 
writePkgHs (pkgname,hprefix) templates wdir mods = do 
  let fn = wdir </> pkgname <.> "hs"
      exportListStr = intercalateWith conncomma ((\x->" module " ++ hprefix ++"."++x).cmModule) mods 
      importListStr = intercalateWith connRet ((\x->"import " ++ hprefix++"."++x).cmModule) mods
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
    if b then return () else createDirectory dir     


copyPredefined :: FilePath -> FilePath -> String -> IO () 
copyPredefined tdir ddir prefix = do 
    notExistThenCreate (ddir </> prefix)
    copyFile (tdir </> "TypeCast.hs" ) (ddir </> prefix </> "TypeCast.hs") 


copyCppFiles :: FilePath -> FilePath -> String -> ClassImportHeader -> IO ()
copyCppFiles wdir ddir cprefix header = do 
  let thfile = cprefix ++ "Type.h"
      hfile = cihSelfHeader header
      cppfile = cihSelfCpp header
  copyFile (wdir </> thfile) (ddir </> thfile) 
  copyFile (wdir </> hfile) (ddir </> hfile) 
  copyFile (wdir </> cppfile) (ddir </> cppfile)

copyModule :: FilePath -> FilePath -> String -> String -> ClassModule -> IO ()
copyModule wdir ddir prefix pkgname mod = do 
  let modbase = cmModule mod 
  let onefilecopy fname = do 
        let (fnamebody,fnameext) = splitExtension fname
            (mdir,mfile) = moduleDirFile fnamebody
            origfpath = wdir </> fname
            (mfile',mext') = splitExtension mfile
            newfpath = ddir </> mdir </> mfile' ++ fnameext   
        -- b <- doesDirectoryExist (ddir</>mdir)
        -- if b then return () else createDirectory (ddir</>mdir)     
        notExistThenCreate (ddir </> mdir) 
        copyFile origfpath newfpath 

  onefilecopy $ prefix <.> modbase ++ ".hs"
  onefilecopy $ prefix <.> modbase ++ ".RawType.hs"
  onefilecopy $ prefix <.> modbase ++ ".FFI.hsc"
  onefilecopy $ prefix <.> modbase ++ ".Interface.hs"
  onefilecopy $ prefix <.> modbase ++ ".Cast.hs"
  onefilecopy $ prefix <.> modbase ++ ".Implementation.hs"
  -- onefilecopy $ prefix <.> modbase ++ ".Existential.hs"

  copyFile (wdir </> pkgname <.> "hs") (ddir </> pkgname <.> "hs")

  return ()
