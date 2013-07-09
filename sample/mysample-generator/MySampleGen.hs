{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid (mempty)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import           Text.StringTemplate hiding (render)
-- 
import           FFICXX.Generate.Code.Cabal
import           FFICXX.Generate.Code.Cpp
import           FFICXX.Generate.Code.Dependency
import           FFICXX.Generate.Config
import           FFICXX.Generate.Code.Cpp
import           FFICXX.Generate.Code.Dependency
import           FFICXX.Generate.Config
import           FFICXX.Generate.Generator.ContentMaker 
import           FFICXX.Generate.Generator.Driver
import           FFICXX.Generate.Type.Annotate
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.PackageInterface
import           FFICXX.Generate.Util
-- 
import qualified FFICXX.Paths_fficxx as F

mysampleclasses = [ ] 

mycabal = Cabal { cabal_pkgname = "MySample" 
                , cabal_cheaderprefix = "MySample"
                , cabal_moduleprefix = "MySample" }

myclass = Class mycabal 

a :: Class 
a = myclass "A" [] mempty Nothing 
    [ Constructor [] Nothing
    , Virtual void_ "Foo" [ ] Nothing
    , Virtual void_ "Foo2" [ long "t" ] Nothing
    ] 

b :: Class 
b = myclass "B" [a] mempty Nothing
    [ Constructor [] Nothing
    , Virtual void_ "Bar" [] Nothing
    ] 

myclasses = [ a, b] 

toplevelfunctions = [ ] 


-- | 
cabalTemplate :: String 
cabalTemplate = "Pkg.cabal"


-- | 
mkCabalFile :: FFICXXConfig
            -> STGroup String  
            -> (TopLevelImportHeader,[ClassModule])
            -> FilePath  
            -> IO () 
mkCabalFile config templates (tih,classmodules) cabalfile = do 
  cpath <- getCurrentDirectory 

  let str = renderTemplateGroup 
              templates 
              [ ("pkgname", "MySample") 
              , ("version",  "0.0") 
              , ("license", "" ) 
              , ("buildtype", "Simple")
              , ("deps", "" ) 
              , ("csrcFiles", genCsrcFiles (tih,classmodules))
              , ("includeFiles", genIncludeFiles "MySample" classmodules) 
              , ("cppFiles", genCppFiles (tih,classmodules))
              , ("exposedModules", genExposedModules "MySample" classmodules) 
              , ("otherModules", genOtherModules classmodules)
              , ("extralibdirs", cpath </> ".." </> "cxxlib" </> "lib" )  -- this need to be changed 
              , ("extraincludedirs", cpath </> ".." </> "cxxlib" </> "include" )  -- this need to be changed 
              , ("extralib", ", mysample")
              , ("cabalIndentation", cabalIndentation)
              ]
              cabalTemplate 
  writeFile cabalfile str




main :: IO ()
main = do 
  putStrLn "generate mysample" 
  cwd <- getCurrentDirectory 

  let cfg =  FFICXXConfig { fficxxconfig_scriptBaseDir = cwd 
                          , fficxxconfig_workingDir = cwd </> "working"
                          , fficxxconfig_installBaseDir = cwd </> "MySample"  
                          } 
      workingDir = fficxxconfig_workingDir cfg
      installDir = fficxxconfig_installBaseDir cfg 
      pkgname = "MySample" 
      (mods,cihs,tih) = mkAll_ClassModules_CIH_TIH ("MySample", (\c->([],[class_name c ++ ".h"]))) (myclasses,toplevelfunctions)
      hsbootlst = mkHSBOOTCandidateList mods 
      cglobal = mkGlobal myclasses 
      summarymodule = "MySample" 
      cabalFileName = "MySample.cabal"
  templateDir <- F.getDataDir >>= return . (</> "template")
  (templates :: STGroup String) <- directoryGroup templateDir 
  -- 
  notExistThenCreate workingDir
  notExistThenCreate installDir 
  notExistThenCreate (installDir </> "src") 
  notExistThenCreate (installDir </> "csrc")
  -- 
  putStrLn "cabal file generation" 
  mkCabalFile cfg templates (tih,mods) (workingDir </> cabalFileName)
  -- 
  putStrLn "header file generation"
  writeTypeDeclHeaders templates workingDir (TypMcro "__MYSAMPLE__") pkgname cihs
  mapM_ (writeDeclHeaders templates workingDir (TypMcro "__MYSAMPLE__") pkgname) cihs
  -- 
  putStrLn "cpp file generation" 
  mapM_ (writeCppDef templates workingDir) cihs
  -- 
  putStrLn "RawType.hs file generation" 
  mapM_ (writeRawTypeHs templates workingDir) mods 
  -- 
  putStrLn "FFI.hsc file generation"
  mapM_ (writeFFIHsc templates workingDir) mods
  -- 
  putStrLn "Interface.hs file generation" 
  mapM_ (writeInterfaceHs mempty templates workingDir) mods
  -- 
  putStrLn "Cast.hs file generation"
  mapM_ (writeCastHs templates workingDir) mods
  -- 
  putStrLn "Implementation.hs file generation"
  mapM_ (writeImplementationHs mempty templates workingDir) mods
  -- 
  putStrLn "hs-boot file generation" 
  mapM_ (writeInterfaceHSBOOT templates workingDir) hsbootlst  
  -- 
  putStrLn "module file generation" 
  mapM_ (writeModuleHs templates workingDir) mods
  -- 
  putStrLn "summary module generation generation"
  writePkgHs summarymodule templates workingDir mods tih
  -- 
  putStrLn "copying"
  copyFileWithMD5Check (workingDir </> cabalFileName)  (installDir </> cabalFileName) 
  -- copyPredefined templateDir (srcDir ibase) pkgname
  copyCppFiles workingDir (csrcDir installDir) pkgname (tih,cihs)
  mapM_ (copyModule workingDir (srcDir installDir) summarymodule) mods 


