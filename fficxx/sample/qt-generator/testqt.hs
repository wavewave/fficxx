{-# LANGUAGE OverloadedStrings #-}

import Foreign.Ptr

import Qt5 

main = do 
  app <- newQApplication 0 nullPtr
  hello <- newQLabel "Hello World"
  qLabelshow hello 
  qApplicationexec app 


