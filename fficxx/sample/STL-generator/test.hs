{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Foreign.C.Types
import Foreign.Ptr

import STL.Vector.Template
import qualified STL.Vector.TH as TH

instance IVector CInt where
  newVector = $(TH.newVector ''CInt "int")
  push_back (Vector ptr) x = $(TH.push_back ''CInt "int") ptr x 

main = do
  v :: Vector CInt <- newVector
  putStrLn "test"
  
