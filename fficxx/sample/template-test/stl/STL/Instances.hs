{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module STL.Instances where

import Foreign.C.Types
import Foreign.Ptr

import  STL
import qualified STL.TH as TH

instance ISTLVector CInt where
  printout (STLVector ptr) = $(TH.printout ''CInt) ptr  
  new = STLVector <$> $(TH.new ''CInt)
  push_back (STLVector ptr) x = $(TH.push_back ''CInt) ptr x 
  at (STLVector ptr) i = $(TH.at ''CInt) ptr i
  delete (STLVector ptr) = $(TH.delete ''CInt) ptr

instance ISTLVector CDouble where
  printout (STLVector ptr) = $(TH.printout  ''CDouble) ptr
  new = STLVector <$> $(TH.new ''CDouble)
  push_back (STLVector ptr) x = $(TH.push_back ''CDouble) ptr x
  at (STLVector ptr) i = $(TH.at ''CDouble) ptr i
  delete (STLVector ptr) = $(TH.delete ''CDouble) ptr


