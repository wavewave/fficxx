{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import GHC           ( runGhc )
import GHC.SourceGen ( HsModule'
                     , (-->)
                     , bvar
                     , funBind
                     , match
                     , module'
                     , putPpr
                     , typeSig
                     , var
                     , wildP
                     )
import GHC.Paths     ( libdir )


constModule :: HsModule'
constModule =
    module' (Just "Const") (Just [var "const"]) []
      [ typeSig "const" $ a --> b --> a
      , funBind "const" $ match [wildP, x] x
      ]
  where
    a = var "a"
    b = var "b"
    x = bvar "x"


main :: IO ()
main = do
  runGhc (Just libdir) $ putPpr constModule
  -- putStrLn "hello world"
