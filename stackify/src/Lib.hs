{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Applicative ((<|>))
import Data.List (stripPrefix)
import Data.Monoid ((<>))
import System.Directory.Tree

data Config = Config
  { inDir  :: FilePath
  , outDir :: FilePath }

makeTemplates :: Config -> IO ()
makeTemplates (Config templateDir destinationDir) = do
  (anchor :/ tree) <- readDirectory templateDir
  case successful tree of
    False -> informUserOfErrors tree
    True -> do
      let flattenedDirs = map (flatten . anchorDirs) $ (contents . sortDir) tree
      print flattenedDirs
      let files = map constructHSFile flattenedDirs
      writeDirectory (destinationDir :/ (Dir destinationDir files))
      print "done"

-- | Build an HSFile by folding a list of DirTrees into a single DirTree
constructHSFile :: Foldable t
                => (FileName, t (DirTree (FileName, String)))
                -> DirTree String
constructHSFile (name', files')
  = foldr (appendHSFile name') (File (name' ++ ".hsfiles") "") files'

-- | qualifies a path to the chosen root
--   in this case, that means a project folder
anchorDirs :: DirTree a
           -> (FileName, [DirTree (FilePath, a)])
anchorDirs (Dir name' contents')
  = (name', map (zipPaths . (name' :/ )) contents')

informUserOfErrors :: DirTree a -> IO ()
informUserOfErrors tree = do
  let _ = map printFailure $ failures tree
  return ()
  where
    printFailure (Failed name' err') = print (name' ++ "failed with" ++ (show err'))

flatten :: Foldable t1 => (FileName, t1 (DirTree a)) -> (FileName, [DirTree a])
flatten (name', arr) = (name', filterFiles $ concatMap (flattenDir) arr)

filterFiles :: [DirTree t] -> [DirTree t]
filterFiles arr = filter isFile arr
  where
    isFile (File _ _) = True
    isFile _ = False

appendHSFile :: FileName
             -> DirTree (FileName, String)
             -> DirTree String
             -> DirTree String
appendHSFile prefix (File shortname' (filename', content')) hsfile =
  File (name hsfile) (file hsfile <> 
    "{-# START_FILE "
   <> mkName filename'
   <> " #-}"
   <> "\n"
   <> content'
   <> "\n\n")
    where
      mkName fName = maybe "failed" id $ stripPrefix (prefix ++ "/") fName
