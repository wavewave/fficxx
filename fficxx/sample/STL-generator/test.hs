{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Foreign.C.Types
import Foreign.Ptr

import STL.Vector.Template
import qualified STL.Vector.TH as TH

instance IVector CInt where
  newVector = Vector <$> $(TH.newVector ''CInt "int")
  push_back (Vector ptr) x = $(TH.push_back ''CInt "int") ptr x
  size (Vector ptr) = $(TH.size ''CInt "int") ptr

main = do
  v :: Vector CInt <- newVector
  n <- size v 
  print =<< size v

  push_back v 1
  print =<< size v
  mapM_ (push_back v) [1..100]
  print =<< size v
  
