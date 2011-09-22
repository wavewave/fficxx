{-# LANGUAGE TypeFamilies, GADTs, ExistentialQuantification, EmptyDataDecls #-}

module HROOT.Class.Existential where

--import Foreign.C            
import Foreign.ForeignPtr
--import Foreign.Marshal.Array


import HROOT.Class.Interface
import HROOT.Class.Implementation ()

data BottomType

class GADTTypeable a where
  data GADTType a :: * -> *
  data EGADTType a :: *

$existEachBody$


