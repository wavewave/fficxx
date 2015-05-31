{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Control.Concurrent
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.StablePtr 
import Foreign.Storable

import System.IO
import Qt5 

-- foreign import ccall "test.h set_string" c_set_string :: Ptr CString -> IO ()

-- foreign import ccall "test.h get_string" c_get_string :: IO (Ptr ())

main = do 
    -- args <- getArgs
    -- argc <-
    -- alloca $ \argv -> do 
    -- c_set_string argv
    -- argv <- c_get_string
  alloca $ \ptr -> do
    poke ptr (0 :: CInt)
    cstr <- newCString "test"
    argv <- newArray [cstr] 
    
    cstrhello <- newCString "Hello world"
    hFlush stdout
 
    putStrLn "point a"
    -- threadDelay 1000000
    app <- newQApplication ptr nullPtr --- 1 argv  -- (castPtr (castStablePtrToPtr argv)) -- (argv :: Ptr CString)
  
    putStrLn "point b"

    hello <- newQLabel cstrhello
    qLabelshow hello 
    qApplicationexec app 


