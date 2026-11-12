{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.PackageYaml where

import Data.Aeson (Value(..), object, (.=),toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither, encodeFile)
import System.FilePath ((</>))

import Domain.ModuleInfo
import Domain.StructureTree

------------------------------------------------------------------------------------------------------

loadPackage :: FilePath -> IO Value
loadPackage projectDir = do
    let packagePath = projectDir </> "package.yaml"
    loadResult <- decodeFileEither packagePath
    case loadResult of
        Left  err -> fail (show err)
        Right val -> return val

savePackage :: FilePath -> Value -> IO ()
savePackage projectDir package = do
    let packagePath = projectDir </> "package.yaml"
    encodeFile packagePath package

------------------------------------------------------------------------------------------------------

addLibrary :: Value -> [T.Text] -> Either String Value
addLibrary package libs = do
    case package of
        Object root -> do
            dependencies <- case KM.lookup "dependencies" root of
                Nothing          -> Right V.empty
                Just (Array arr) -> Right arr
                Just _           -> Left  "dependencies isnt an Array"
            let updatedDependencies = foldl addOneLibrary dependencies libs
            let updatedRoot = KM.insert "dependencies" (Array updatedDependencies) root
            Right (Object updatedRoot)
        _           -> Left "package isnt an Object"


addOneLibrary :: V.Vector Value -> Text -> V.Vector Value
addOneLibrary dependencies lib = V.snoc dependencies (String $ lib)

