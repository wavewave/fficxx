{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString.Char8 as B

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String


import           STD.CppString
import           STD.UniquePtr.Template
import qualified STD.UniquePtr.TH as TH

$(TH.genUniquePtrInstanceFor ''CppString "string")

main = do
  putStrLn "test"
