{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction, 
    ScopedTypeVariables, UndecidableInstances #-}
{- |
Functions I found useful for doing webapps with HStringTemplate.

More usage examples can be found by grep -r \"Text.StringTemplate.Helpers\" in happs-tutorial, on hackage. 
-}
module Text.StringTemplate.Helpers  (
  directoryGroups'
  , directoryGroups
  -- uses function from HStringTemplate which isn't strict, and I believe should be fixed in distribution.
  , directoryGroupsOld 
  , dirgroupKeys
  , getTemplateGroup
  , renderTemplateDirGroup
  , lookupDirgroup
  , renderTemplateGroup
  , render1
  , STDirGroups
  , readTmplDef
  , readTmplM
  , readTmplTuples
  , badTmplVarName
  , directoryGroupNew'
)

where

import Text.StringTemplate 
import Text.StringTemplate.Base
import System.Directory
import System.FilePath
import qualified System.IO.Strict as Strict
import Control.Applicative
import Data.List (find)
import Data.Char
import Control.Monad.Reader
import HSH (bracketCD)
import qualified Data.Map as M
import Text.StringTemplate.Classes
import Safe
import qualified System.FilePath.Find as Find
import System.FilePath
{- | 
Chooses a template from an STGroup, or errors if not found.

Render that template using attrs.

If a template k/v pair is repeated, it appears twice. (Perhaps a clue to buggy behavior?)

Repeated keys could be eliminated by running clean:

> clean = nubBy (\(a1,b1) (a2,b2) -> a1 == a2) . sortBy (\(a1,b1) (a2,b2) -> a1 `compare` a2)

The ToSElem type is probably either String or [String]
-}
renderTemplateGroup :: (ToSElem a) => STGroup String -> [(String, a)] -> [Char] -> String
renderTemplateGroup gr attrs tmpl = 
       maybe ( "template not found: " ++ tmpl )
             ( toString . setManyAttribSafer attrs )
             ( getStringTemplate tmpl gr )

renderTemplateGroupS :: STGroup String -> [(String, String)] -> [Char] -> String
renderTemplateGroupS = renderTemplateGroup

-- can this be done for Bytestrings? Below doesn't work, need an instance for (ToSElem B.ByteString)
--renderTemplateGroupB :: STGroup String -> [(String, B.ByteString)] -> [Char] -> String
--renderTemplateGroupB = renderTemplateGroup

--t :: IO [FilePath]
--t = do (map :: M.Map FilePath (STGroup String)) <- ( directoryGroupsSafer "/home/thartman/testtemplates" )
--       return $ M.keys map


--getST

type STDirGroups a = M.Map FilePath (STGroup a) 


{- | 
Helper function to calculate a map of directory groups from a top-level directory

Each directory gives rise to its own groups.

Groups are independent; groups from higher in the directory structure do not have access to groups lower.

The top group has key \".\" (mnemonic, current directory), other groups have key names of subdirectories, including the starting ., eg \".\/templates\/path\/to/\subdir\"
-} 
directoryGroups' :: (FilePath -> IO a) -> FilePath -> IO (M.Map FilePath a)
directoryGroups' f' d = bracketCD d $ do
                           subDirs <- findDirectories $ "." 
                           return . M.fromList =<< mapM f subDirs                               
  where 
    f d = do g <- f' d
             return (d,g)
    findDirectories d = Find.find Find.always (Find.fileType Find.==? Find.Directory) d

{- | Non-strict. I'm pretty sure this is wrong. Based on default directoryGroup function in HStringTemplate package -}
directoryGroupsOld :: (Stringable a) => FilePath -> IO (M.Map FilePath (STGroup a))
directoryGroupsOld = directoryGroups' directoryGroup 

{- | Strict directoryGroups, which is the right thing.
-}
directoryGroups :: (Stringable a) => FilePath -> IO (M.Map FilePath (STGroup a))
directoryGroups = directoryGroups' directoryGroupNew

{- | I'm  this does the same thing as directoryGroup (modulo IO strictness), 
     which uses an applicative idiom that melts my brain. 
     Not a direct translation, but it's easier for me to understand when written
     Important change: readFile is strict. If it is left lazy, appkiller.sh causes happstutorial to crash
     when in dynamicTemplateReload mode. (See HAppS GoogleGroup.)
     I think this needs to be fixed in HStringTemplate distribution as well.
     This should be fixed in HSTringTemplate package as well.
-}
directoryGroupNew :: (Stringable a) => FilePath -> IO (STGroup a)
directoryGroupNew = directoryGroupNew' (\_ -> False) (\_ -> False)

{- | directoryGroup helper function for more flexibility, and rewritten to use
     do notation rather than applicative style that melted my brain.
     
     ignoreTemplate specifies a filter for templates that should be skipped, eg backup files etc.

     errorTemplate specifies a filter which will cause function to fail.

  > directoryGroupHAppS = directoryGroupNew' ignoret badTmplVarName
  > where ignoret f = not . null . filter (=='#') $ f 
-}
directoryGroupNew' :: (Stringable a) =>
                      (FilePath -> Bool)
                      -> (String -> Bool)
                      -> FilePath
                      -> IO (STGroup a)
directoryGroupNew' ignoreTemplate errorTemplate path = do
    fs1 <- return . ( filter filt ) =<< getDirectoryContents path
    fs <- mapM errT =<< return fs1
    templates <- mapM readTemplate fs
    stmapping <- return . zip (map dropExtension fs) $ templates
    return $ groupStringTemplates stmapping
  where
    readTemplate f = do 
               contents <- Strict.readFile $ path </> f
               return . newSTMP $ contents
    errT t = if ( errorTemplate . takeBaseName ) t 
               then fail $ "directoryGroupNew', bad template name: " ++ t 
               else return t
    filt f = ( ( (".st" ==) . takeExtension ) $ f)
           && ( (not . ignoreTemplate) $ f)



{- | The STGroup can't be shown in a useful way because it's a function type, but you can at least show the directories via Data.Map.keys.
-}
dirgroupKeys :: (Stringable a) => STDirGroups a -> [FilePath]
dirgroupKeys = M.keys

lookupDirgroup :: (Stringable a) => FilePath -> STDirGroups a -> Maybe (STGroup a)
lookupDirgroup d = M.lookup d



-- | > example: getTG "./baselayout" ts'
getTemplateGroup :: (Stringable a) => FilePath -> STDirGroups a -> STGroup a
getTemplateGroup dir tdg = maybe (error $ "getTG, bad dir:" ++ dir) id . lookupDirgroup dir $ tdg

-- | > example: renderTemplateDirGroup ts' "./baselayout" "base" 
renderTemplateDirGroup :: ToSElem a => STDirGroups String -> FilePath -> String -> [(String,a)] -> String
renderTemplateDirGroup tdg dir tname attrs = 
  let ts = getTemplateGroup dir tdg 
  in  renderTemplateGroup ts attrs tname

setManyAttribSafer attrs  st = 
    let mbFoundbadattr = find badTmplVarName . map fst $ attrs
    in  maybe (setManyAttrib attrs st)
              (\mbA -> newSTMP . ("setManyAttribSafer, bad template atr: "++) $ mbA)
              mbFoundbadattr

(<$$>) :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) = (<$>) . (<$>)

{- Check for a variable that will cause errors and heartache using HStringTemplate -}
badTmplVarName :: String -> Bool
badTmplVarName t = not . null . filter (not . isAlpha) $ t

{- | 
> render1 [("name","Bill")] "Hi, my name is $name$"
> render1 attribs tmpl = render . setManyAttrib attribs . newSTMP $ tmpl
-}
render1 :: [(String,String)] -> String -> String
render1 attribs tmpl = render . setManyAttrib attribs . newSTMP $ tmpl

-- useful for HAppS, eg for dynamic menus
readTmplTuples :: STGroup String -> String -> [(String, String)]
readTmplTuples = readTmplDef [("readTutTuples error","")]

readTmplDef :: (Read b) => b -> STGroup String -> FilePath -> b
readTmplDef def ts f = either (const def) id ( readTmplM ts f :: Read a => Either String a)

readTmplM :: (Monad m, Read a) => STGroup String -> FilePath -> m a
readTmplM ts file = safeRead . renderTemplateGroup ts ([] :: [(String,String)] ) . concatMap escapequote $ file
  where escapequote char = if char=='"' then "\\\"" else [char]

safeRead :: (Monad m, Read a) => String -> m a
safeRead s = maybe (fail $ "safeRead: " ++ s) return . readMay $ s


