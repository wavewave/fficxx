{-# LANGUAGE OverloadedStrings #-}

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.String

import Qt5 

main = do 
  cstr <- newCString "test" 
  null <- newCString ""
  arr <- newArray0 null [cstr] 
  app <- newQApplication 0 arr
  hello <- newQLabel "Hello World"
  qLabelshow hello 
  qApplicationexec app 


