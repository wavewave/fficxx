{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Control.Concurrent
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import System.Environment
--
import Qt5 


main = do 
  args <- getArgs
  let n = length args
  alloca $ \p_argc -> do
    poke p_argc (fromIntegral n)
    p_argv <- newArray =<<  mapM newCString args
    app <- newQApplication p_argc p_argv 
    cstr <- newCString "Hello world"
    hello <- newQLabel cstr
    show1 hello 
    exec app 


