{-# LANGUAGE OverloadedStrings #-}
module Domain.ModuleInfo where

import Data.Text (Text,pack,unpack,intercalate)
import System.FilePath ((</>))

import Domain.StructureTree
import GHC.Base (Module(Module))
import GHC.IO.Handle.Lock (FileLockingNotSupported(FileLockingNotSupported))

data ModuleInfo = ModuleInfo
    { moduleName :: Text
    , implPath   :: FilePath
    , testPath   :: FilePath
    } deriving (Show)

getModuleinfoFromTree :: StructureNode -> [ModuleInfo]
getModuleinfoFromTree tree = go [] (unpack $ name tree) tree
    where 
        go dir root node = 
            case node of
            ProjectNode _ _ children ->
                concatMap (go dir root) children

            DirectoryNode name _ children ->
                concatMap (go (dir ++ [unpack name]) root) children

            ModuleNode name _ _ ->
                let
                    modulePath = foldl (</>) root dir
                                 </> (unpack name ++ ".hs")

                    testFilePath = (root </> "test") 
                                   </> (unpack name ++ "Spec.hs")
                in
                    [ModuleInfo
                        { moduleName = intercalate "." (map pack dir ++ [name])
                        , implPath = modulePath
                        , testPath = testFilePath
                        }]

            _ -> []