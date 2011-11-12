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

-- Modules

-- | Generate HROOT.cabal file 

-- | Generate Existential.hs file 

----------

----

----

writeTypeDeclHeaders :: STGroup String -> ClassGlobal 
                     -> FilePath -> [ClassImportHeader]
                     -> IO ()
writeTypeDeclHeaders templates cglobal wdir headers = do 
  let fn = wdir </> "HROOTType.h"
      classes = map cihClass headers
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkTypeDeclHeader templates cglobal classes)

writeDeclHeaders :: STGroup String -> ClassGlobal 
                 -> FilePath -> ClassImportHeader
                 -> IO () 
writeDeclHeaders templates cglobal wdir header = do 
  let fn = wdir </> cihSelfHeader header
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkDeclHeader templates cglobal header)

writeCppDef :: STGroup String -> FilePath -> ClassImportHeader -> IO () 
writeCppDef templates wdir header = do 
  let fn = wdir </> cihSelfCpp header
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkDefMain templates header)

writeRawTypeHs :: STGroup String -> FilePath -> ClassModule -> IO ()
writeRawTypeHs templates wdir mod = do
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> rawtypeHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkRawTypeHs templates mod) 

writeFFIHsc :: STGroup String -> FilePath -> ClassModule -> IO ()
writeFFIHsc templates wdir mod = do 
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> ffiHscFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkFFIHsc templates mod)

writeInterfaceHs :: AnnotateMap -> STGroup String -> FilePath -> ClassModule 
                 -> IO ()
writeInterfaceHs amap templates wdir mod = do 
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> interfaceHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkInterfaceHs amap templates mod)

writeCastHs :: STGroup String -> FilePath -> ClassModule 
            -> IO ()
writeCastHs templates wdir mod = do 
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> castHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkCastHs templates mod)

writeImplementationHs :: AnnotateMap -> STGroup String -> FilePath -> ClassModule 
                      -> IO ()
writeImplementationHs amap templates wdir mod = do 
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> implementationHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkImplementationHs amap templates mod)

writeExistentialHs :: STGroup String -> ClassGlobal -> FilePath -> ClassModule 
                   -> IO ()
writeExistentialHs templates cglobal wdir mod = do 
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> existentialHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkExistentialHs templates cglobal mod)


writeModuleHs :: STGroup String -> FilePath -> ClassModule -> IO () 
writeModuleHs templates wdir mod = do 
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> "hs"
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkModuleHs templates mod)

writeHROOTHs :: STGroup String -> FilePath -> [ClassModule] -> IO () 
writeHROOTHs templates wdir mods = do 
  let fn = wdir </> "HROOT.hs"
      exportListStr = intercalateWith conncomma ((" module HROOT.Class."++).cmModule) mods 
      importListStr = intercalateWith connRet (("import HROOT.Class."++).cmModule) mods
      str = renderTemplateGroup 
              templates 
              [ ("exportList", exportListStr) 
              , ("importList", importListStr) ]
              "HROOT.hs"
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h str


copyPredefined :: FilePath -> FilePath -> IO () 
copyPredefined tdir ddir = do 
  copyFile (tdir </> "TypeCast.hs" ) (ddir </> "HROOT/TypeCast.hs") 

copyCppFiles :: FilePath -> FilePath -> ClassImportHeader -> IO ()
copyCppFiles wdir ddir header = do 
  let thfile = "HROOTType.h"
      hfile = cihSelfHeader header
      cppfile = cihSelfCpp header
  copyFile (wdir </> thfile) (ddir </> thfile) 
  copyFile (wdir </> hfile) (ddir </> hfile) 
  copyFile (wdir </> cppfile) (ddir </> cppfile)

copyModule :: FilePath -> FilePath -> ClassModule -> IO ()
copyModule wdir ddir mod = do 
  let modbase = cmModule mod 
  let onefilecopy fname = do 
        let (fnamebody,fnameext) = splitExtension fname
            (mdir,mfile) = moduleDirFile fnamebody
            origfpath = wdir </> fname
            (mfile',mext') = splitExtension mfile
            newfpath = ddir </> mdir </> mfile' ++ fnameext   

        b <- doesDirectoryExist (ddir</>mdir)
        if b then return () else createDirectory (ddir</>mdir)     
        copyFile origfpath newfpath 

  onefilecopy $ "HROOT.Class." ++ modbase ++ ".hs"
  onefilecopy $ "HROOT.Class." ++ modbase ++ ".RawType.hs"
  onefilecopy $ "HROOT.Class." ++ modbase ++ ".FFI.hsc"
  onefilecopy $ "HROOT.Class." ++ modbase ++ ".Interface.hs"
  onefilecopy $ "HROOT.Class." ++ modbase ++ ".Cast.hs"
  onefilecopy $ "HROOT.Class." ++ modbase ++ ".Implementation.hs"
  -- onefilecopy $ "HROOT.Class." ++ modbase ++ ".Existential.hs"

  copyFile (wdir </> "HROOT.hs") (ddir </> "HROOT.hs")

  return ()
