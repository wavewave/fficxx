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
cabal-version:  >=1.6
Extra-source-files: CHANGES
                    Config.hs
                    csrc/HROOT.h
                    csrc/addon.h
                    example/histadd.hs
                    example/random1d.hs
                    example/random2d.hs
                    example/random2dApp.hs
                    example/saveAsROOT.hs
Source-repository head
  type:     darcs
  location: http://ianwookim.org/public/repo/HROOT


Library
  hs-source-dirs: src
  ghc-options:  -Wall -funbox-strict-fields -fno-warn-unused-do-bind -fno-warn-orphans
  ghc-prof-options: -caf-all -auto-all
  Build-Depends:      base>4 && < 5
  Exposed-Modules:    HROOT
                      HROOT.Class.Interface
                      HROOT.Class.Implementation
                      HROOT.Class.FFI
                      HROOT.AddOn
                      HROOT.AddOnFunction
  extra-libraries:    stdc++
  Extensions:	      ForeignFunctionInterface
  Include-dirs:       csrc
  Install-includes:   HROOT.h
                      addon.h
  C-sources:          csrc/HROOT.cpp
                      csrc/addon.cpp
   


