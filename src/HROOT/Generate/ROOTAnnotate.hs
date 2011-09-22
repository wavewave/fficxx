{-# LANGUAGE QuasiQuotes #-}

module HROOT.Generate.ROOTAnnotate where

import HROOT.Generate.Type.Annotate 
import HROOT.Generate.QQ.Verbatim


import qualified Data.Map as M

annotateMap :: AnnotateMap 
annotateMap = M.fromList [ tNamedAnn, tNamedNewAnn, tNamedSetTitleAnn
                         , tObjectGetNameAnn, tObjectDrawAnn, tObjectFindObjectAnn ] 


tNamedAnn = ((HROOTClass,"TNamed"),[verbatim|
Class TNamed
reference : http://root.cern.ch
|])

tNamedNewAnn = ((HROOTMethod,"newTNamed"),[verbatim|constructor : 

> TNamed( char* name, char* title) 

|] )

tNamedSetTitleAnn = ((HROOTMethod, "setTitle"), [verbatim|SetTitle method

> SetTitle( char* name, char* title ) 

|] )


tObjectGetNameAnn = ((HROOTMethod, "getName"), [verbatim|
> char* TObject::GetName()

|])

tObjectDrawAnn = ((HROOTMethod, "draw"), [verbatim|
> void TObject::Draw( char* option )

|])

tObjectFindObjectAnn = ((HROOTMethod, "findObject"), [verbatim|
> TObject* TObject::FindObject( char* name )

|])


