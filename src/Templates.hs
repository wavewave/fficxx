module Templates where

import System.FilePath ((</>))

scriptBaseDir = "/home/wavewave/nfs/workspace/HROOT-generate" 
templateDir   = scriptBaseDir </> "template"
workingDir    = scriptBaseDir </> "working"

declarationTemplate = "declaration.h"
declbodyTemplate    = "declbody.h"
funcdeclTemplate    = "funcdecl.h" 

classDefTemplate   = "classdef.cpp"
definitionTemplate = "definition.cpp"
functionTemplate   = "function.cpp" 
funcbodyTemplate   = "functionbody.cpp"

headerFileName = "HROOT.h"
cppFileName = "HROOT.cpp" 
hscFileName = "Function.hsc"
hsFileName  = "Class.hs"
