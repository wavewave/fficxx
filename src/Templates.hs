module Templates where

import System.FilePath ((</>))

{- 
scriptBaseDir :: FilePath
scriptBaseDir = "/home/wavewave/nfs/prog/HROOT-generate" 

templateDir :: FilePath
templateDir   = scriptBaseDir </> "template"

workingDir :: FilePath
workingDir    = scriptBaseDir </> "working"

installBaseDir :: FilePath
installBaseDir = "/home/wavewave/nfs/prog/HROOT"

srcDir :: FilePath -> FilePath
srcDir installbasedir = installbasedir </> "src" </> "HROOT"

csrcDir :: FilePath -> FilePath
csrcDir installbasedir = installbasedir </> "csrc" 

declarationTemplate :: String
declarationTemplate = "HROOT.h"

declbodyTemplate :: String
declbodyTemplate    = "declbody.h"

funcdeclTemplate :: String
funcdeclTemplate    = "funcdecl.h" 

definitionTemplate :: String
definitionTemplate = "HROOT.cpp"

classDefTemplate :: String
classDefTemplate   = "classdef.cpp"

functionTemplate :: String
functionTemplate   = "function.cpp" 

funcbodyTemplate :: String
funcbodyTemplate   = "functionbody.cpp"

headerFileName :: String
headerFileName = "HROOT.h"

cppFileName :: String
cppFileName = "HROOT.cpp" 

hscFileName :: String
hscFileName = "Function.hsc"

hsFileName :: String
hsFileName  = "Class.hs"

typeHsFileName :: String
typeHsFileName = "Type.hs"