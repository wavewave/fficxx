{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -w #-}

--test = do
--  addModFinalizer (addForeignSource LangCxx "\n#include \"test\"")

module MyModule where

data K = K Int

test :: IO ()

foreign import ccall safe "ProxyTestLoader.h Loader_delete" c_loader_delete :: Ptr RawLoader-> IO ()
