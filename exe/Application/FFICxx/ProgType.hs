{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Application.FFICxx.ProgType
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : BSD3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Program options for 'build'
--

module Application.FFICxx.ProgType where

import System.Console.CmdArgs

data FFICxx = Generate { configfile :: FilePath } 
              deriving (Show,Typeable,Data)

generate :: FFICxx
generate = Generate { configfile = "" &= typ "CONFIGFILE" &= argPos 0 }

mode :: FFICxx
mode = modes [generate]
