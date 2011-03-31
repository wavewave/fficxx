import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import System.FilePath ((</>))
import System.Posix.Files 

import System.Directory
--import System.Process

isCabal :: String -> Bool 
isCabal str 
  | length str > 6 = let ext = reverse . take 6 . reverse $ str in ext == ".cabal" 
  | otherwise = False
                
cabalName :: Parser String 
cabalName = do 
  manyTill anyChar (try (string "Name:"))
  spaces 
  many1 (noneOf " \n")

cabalVersion :: Parser String
cabalVersion = do
  manyTill anyChar (try (string "Version:"))
  spaces
  many1 (oneOf "0123456789.")
    
nameVersion :: Parser (String,String) 
nameVersion = do 
  n <- cabalName 
  v <- cabalVersion 
  return (n,v)
  
main = do 
  putStrLn "version check"
  currdir <- getDirectoryContents "."
  let cabalfile = head $ filter isCabal currdir
  str <- readFile cabalfile 
  let Right (name,version) = parse nameVersion "" str
      filename = name ++ "-" ++ version
      linkbase = "/home/wavewave/nfs/doc/prog" 
      docbase  = "/home/wavewave/nfs/usr/share/doc"
      linkpath = linkbase </> name 
      origpath = docbase </> filename
  
  putStrLn $ "ln -s " ++ origpath ++ " " ++ linkpath
  
  test <- getDirectoryContents linkbase  
  
  if elem name test 
    then do 
      putStrLn "removing link"
      removeLink linkpath  
    else do 
      putStrLn "doesn't exist" 
      return () 
{-  putStrLn $ show test
  b <- fileExist linkpath 
  if b 
     then do 
       putStrLn $ "removing link" 
       removeLink linkpath 
     else do
       putStrLn $ "doesn't exist"
       return () -}
 {-  readProcess "ln" ["-s", origpath, linkpath] "" -} 
  createSymbolicLink origpath linkpath
    
  return () 
  
