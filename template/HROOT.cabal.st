Name:		HROOT
Version:	$version$
Synopsis:	Wrapper for ROOT
Description: 	Haskell FFI binding for ROOT  
Homepage:       http://ianwookim.org/HROOT
License: 	LGPL-2.1
License-file:	LICENSE
Author:		Ian-Woo Kim
Maintainer: 	Ian-Woo Kim <ianwookim@gmail.com>
Category:       Graphics
Tested-with:    GHC >= 7.0.2 && <= 7.0.3
Build-Type: 	Custom
cabal-version:  >=1.10
Extra-source-files: 
$cabalIndentation$CHANGES
$cabalIndentation$Config.hs
$cabalIndentation$csrc/HROOT.h
$cabalIndentation$csrc/addon.h
$includeFilesWithCsrc$
$cabalIndentation$example/histadd.hs
$cabalIndentation$example/random1d.hs
$cabalIndentation$example/random2d.hs
$cabalIndentation$example/random2dApp.hs
$cabalIndentation$example/saveAsROOT.hs
$cabalIndentation$example/datetime.hs
$cabalIndentation$example/project1d.hs
$cabalIndentation$test/compiletest.hs

Source-repository head
  type:     darcs
  location: http://ianwookim.org/public/repo/HROOT

-- Test-Suite compiletest
--   default-language: Haskell2010
--   type: exitcode-stdio-1.0
--   main-is: compiletest.hs
--   build-depends: base>4, base<5, HROOT==$version$
--   hs-source-dirs: test

Library
  default-language: Haskell2010
  hs-source-dirs: src
  ghc-options:  -Wall -funbox-strict-fields -fno-warn-unused-do-bind -fno-warn-orphans
  ghc-prof-options: -caf-all -auto-all
  Build-Depends:      base>4 && < 5
  Exposed-Modules:
$cabalIndentation$HROOT
$cabalIndentation$HROOT.Types  
$exposedModules$  
  Other-Modules:
$otherModules$

--                      HROOT
--                      HROOT.Class.Interface
--                      HROOT.Class.Implementation
--                      HROOT.Class.FFI
--                      HROOT.AddOn
--                      HROOT.AddOnFunction
  extra-libraries:    stdc++
-- default-extensions:	      ForeignFunctionInterface
  Include-dirs:       csrc
  Install-includes:   
$cabalIndentation$addon.h
$cabalIndentation$HROOT.h
$includeFiles$
  C-sources:          
$cppFiles$

-- csrc/HROOT.cpp
-- csrc/addon.cpp
   


