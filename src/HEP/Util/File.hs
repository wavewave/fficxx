module HEP.Util.File where

import Data.List 
import Data.List.Split

moduleDirFile :: String -> (String,String)
moduleDirFile mname = 
  let splitted = splitOn "." mname
      moddir  = intercalate "/" (init splitted )
      modfile = (last splitted) ++ ".hs" 
  in  (moddir, modfile)
