module Application.FFICxx.Job where

import Bindings.Cxx.Generate.Config

import System.IO
import Text.Parsec

startGenerateJob :: FilePath -> IO () 
startGenerateJob conf = do 
  putStrLn "job started"
  putStrLn "Automatic HROOT binding generation" 
  str <- readFile conf 
  let config = case (parse fficxxconfigParse "" str) of 
                 Left msg -> error (show msg)
                 Right ans -> ans

  putStrLn $ show config 