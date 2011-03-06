module Templates where

import System.FilePath ((</>))

scriptBaseDir = "/home/wavewave/nfs/prog/HROOT-generate" 
templateDir   = scriptBaseDir </> "template"
workingDir    = scriptBaseDir </> "working"
installBaseDir = "/home/wavewave/nfs/prog/HROOT"
srcDir       = installBaseDir </> "src" </> "HROOT"
csrcDir       = installBaseDir </> "csrc" 


declarationTemplate = "HROOT.h"
declbodyTemplate    = "declbody.h"
funcdeclTemplate    = "funcdecl.h" 

definitionTemplate = "HROOT.cpp"
classDefTemplate   = "classdef.cpp"
functionTemplate   = "function.cpp" 
funcbodyTemplate   = "functionbody.cpp"

headerFileName = "HROOT.h"
cppFileName = "HROOT.cpp" 
hscFileName = "Function.hsc"
hsFileName  = "Class.hs"
typeHsFileName = "Type.hs"