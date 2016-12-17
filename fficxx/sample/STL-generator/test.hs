{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

import Foreign.C.Types
import Foreign.Ptr

import STL.Vector.Template
import qualified STL.Vector.TH as TH

instance IVector CInt where
  push_back (Vector ptr) x = $(TH.push_back ''CInt) ptr x 

main = do
  putStrLn "test"
