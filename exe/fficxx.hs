{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Executable  : fficxx
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Generate source code for c++-haskell FFI 
--

module Main where

import System.Console.CmdArgs

import Application.FFICxx.ProgType
import Application.FFICxx.Command


main :: IO () 
main = do 
  param <- cmdArgs mode
  putStrLn $ show param
  commandLineProcess param 

