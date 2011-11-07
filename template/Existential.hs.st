{-# LANGUAGE TypeFamilies, GADTs, ExistentialQuantification, EmptyDataDecls #-}

-- module HROOT.Class.Existential where

$existHeader$


--import Foreign.C            
import Foreign.ForeignPtr
--import Foreign.Marshal.Array


import HROOT.Class.$modname$.Interface
import HROOT.Class.$modname$.Implementation ()

data BottomType

class GADTTypeable a where
  data GADTType a :: * -> *
  data EGADTType a :: *

$existEachBody$


