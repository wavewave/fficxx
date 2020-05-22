{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell          #-}

module Test.TemplateInstantiation.B where

import           Foreign.C.Types             (CInt(..))
--
import           FFICXX.Runtime.TH           (IsCPrimitive (..),
                                              TemplateParamInfo (..))
import           STD.Vector.Template
import qualified STD.Vector.TH               as TH
import           STD.VectorIterator.Template
import qualified STD.VectorIterator.TH       as TH


TH.genVectorIteratorInstanceFor
  CPrim
  ( [t|CInt|], TPInfo { tpinfoCxxType       = "int"
                      , tpinfoCxxHeaders    = []
                      , tpinfoCxxNamespaces = []
                      , tpinfoSuffix        = "int"
                      }
  )

TH.genVectorInstanceFor
  CPrim
  ( [t|CInt|], TPInfo { tpinfoCxxType       = "int"
                      , tpinfoCxxHeaders    = []
                      , tpinfoCxxNamespaces = []
                      , tpinfoSuffix        = "int"
                      }
  )

test :: IO ()
test = do
  v <- newVector :: IO (Vector CInt)
  n <- size v
  print n
  push_back v 10
  push_back v 11
  n' <- size v
  print n'
