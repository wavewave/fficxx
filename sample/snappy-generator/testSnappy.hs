
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<$>),(<*>),pure)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as BU


import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable as S

import System.Environment

import Snappy


main :: IO ()
main = do
  putStrLn "test Snappy"

  args <- getArgs
  when (length args /= 1) $ do
    error "./testSnappy [filename]"
  bstr <- B.readFile (head args)

  {-
  (obstr,len) <- BU.unsafeUseAsCString bstr $ \cstr -> do
                      p_ostr <- mallocArray 1000
                      p_len <- malloc :: IO (Ptr CULong)
                      -----------------------------
                      rawCompress cstr 1000  p_ostr  p_len
                      -----------------------------
                      len <- S.peek p_len
                      putStrLn $ "compressed size = " ++ (show len)
                      (,) <$> BU.unsafePackCString p_ostr <*> pure len  -}
  p_ostr <- mallocArray 2000
  p_len <- malloc :: IO (Ptr CULong)
  rawCompress bstr 2000 p_ostr p_len
  len <- S.peek p_len
  putStrLn $ "compressed size = " ++ (show len)
  (obstr,len) <- (,) <$> BU.unsafePackCString p_ostr <*> pure len


  rstr <- BU.unsafeUseAsCString obstr $ \cstr -> do
            p_ostr <- mallocArray 10000
            --------------------
            b <- rawUncompress cstr len p_ostr
            --------------------
            putStrLn $ "success? " ++ show b
            BU.unsafePackCString p_ostr

  B.putStrLn rstr

  return ()
