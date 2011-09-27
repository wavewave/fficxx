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
Extra-source-files: CHANGES
                    Config.hs
                    csrc/HROOT.h
                    csrc/addon.h
                    example/histadd.hs
                    example/random1d.hs
                    example/random2d.hs
                    example/random2dApp.hs
                    example/saveAsROOT.hs
                    test/compiletest.hs
Source-repository head
  type:     darcs
  location: http://ianwookim.org/public/repo/HROOT

Test-Suite compiletest
  default-language: Haskell2010
  type: exitcode-stdio-1.0
  main-is: compiletest.hs
  build-depends: base>4, base<5, HROOT==$version$
  hs-source-dirs: test

Library
  default-language: Haskell2010
  hs-source-dirs: src
  ghc-options:  -Wall -funbox-strict-fields -fno-warn-unused-do-bind -fno-warn-orphans
  ghc-prof-options: -caf-all -auto-all
  Build-Depends:      base>4 && < 5
  Exposed-Modules:  
$exposedModules$  

--                      HROOT
--                      HROOT.Class.Interface
--                      HROOT.Class.Implementation
--                      HROOT.Class.FFI
--                      HROOT.AddOn
--                      HROOT.AddOnFunction
  extra-libraries:    stdc++
-- default-extensions:	      ForeignFunctionInterface
  Include-dirs:       csrc
  Install-includes:   HROOT.h
                      addon.h
  C-sources:          csrc/HROOT.cpp
                      csrc/addon.cpp
   


