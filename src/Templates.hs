module Templates where

import System.FilePath ((</>))

scriptBaseDir = "/home/wavewave/nfs/workspace/HROOT-generate" 
templateDir   = scriptBaseDir </> "template"
workingDir    = scriptBaseDir </> "working"

declarationTemplate = "declaration.h"
declbodyTemplate    = "declbody.h"
funcdeclTemplate    = "funcdecl.h" 


definitionTemplate = "definition.cpp"
functionTemplate   = "function.cpp" 
funcbodyTemplate   = "functionbody.cpp"
