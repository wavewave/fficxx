module Main where

import Language.Haskell.GHC.ExactPrint
  ( makeDeltaAst,
    parseModule,
    showAst,
  )

main :: IO ()
main = do
  e <- parseModule "/nix/store/1dccaqdx3v2acc4zk5cnln14jf5q7h04-ghc-9.6.2-with-packages/lib/ghc-9.6.2/lib" "./sample.hs"
  case e of
    Left msg -> print "error" -- print msg
    Right parsed -> putStrLn (showAst (makeDeltaAst parsed))
