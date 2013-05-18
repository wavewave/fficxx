Name:		$pkgname$
Version:	$version$
Synopsis:	Wrapper for ROOT
Description: 	Haskell FFI binding for ROOT  
Homepage:       http://ianwookim.org/HROOT
License: 	LGPL-2.1
License-file:	LICENSE
Author:		Ian-Woo Kim
Maintainer: 	Ian-Woo Kim <ianwookim@gmail.com>
Category:       Graphics
Tested-with:    GHC >= 7.0
Build-Type: 	Custom
cabal-version:  >=1.10
Extra-source-files: 
$cabalIndentation$CHANGES
$cabalIndentation$Config.hs
$csrcFiles$

Source-repository head
  type:     git
  location: https://github.com/wavewave/HROOT, https://github.com/wavewave/HROOT-generate

Library
  default-language: Haskell2010
  hs-source-dirs: src
  ghc-options:  -Wall -funbox-strict-fields -fno-warn-unused-do-bind -fno-warn-orphans
  ghc-prof-options: -caf-all -auto-all
  Build-Depends:      base>4 && < 5, fficxx-runtime >= 0.0.999 $deps$
  Exposed-Modules:
$exposedModules$  
  Other-Modules:
$otherModules$

  extra-libraries:    stdc++
  Include-dirs:       csrc
  Install-includes:   
$includeFiles$
  C-sources:          
$cppFiles$

   


