{-# LANGUAGE TypeFamilies, GADTs, ExistentialQuantification, EmptyDataDecls #-}

-- module HROOT.Class.Existential where

$existHeader$


--import Foreign.C            
import Foreign.ForeignPtr
--import Foreign.Marshal.Array
import HROOT.TypeCast
import HROOT.Class.TClass.RawType
import HROOT.Class.TClass.Interface
import HROOT.Class.TClass.Cast
import HROOT.Class.TClass.Implementation
import HROOT.Class.TObject.RawType
import HROOT.Class.TObject.Interface
import HROOT.Class.TObject.Cast
import HROOT.Class.TObject.Implementation

$existImport$

-- import HROOT.Class.$modname$.Interface
-- import HROOT.Class.$modname$.Implementation ()

$existEachBody$


