{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.PackageYaml where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither, encodeFile)
import System.FilePath ((</>))

import Domain.ModuleInfo
import Domain.StructureTree

loadPackage :: StructureNode -> IO Value
loadPackage tree = do
    let projectName = name tree
    let packagePath = T.unpack projectName </> "package.yaml"
    loadResult <- decodeFileEither packagePath
    case loadResult of
        Left  err -> fail (show err)
        Right val -> return val

addTestSuites :: Text -> Value -> [ModuleInfo] -> Either String Value
addTestSuites projectName package moduleInfo =
    case package of
        Object root -> do
            tests <- case KM.lookup "tests" root of
                Nothing -> Right KM.empty
                Just (Object obj) -> Right obj
                Just _ -> Left "tests isnt an Object"
            let updatedTests = foldl (addOneSuite projectName) tests moduleInfo
            let updatedRoot = KM.insert "tests" (Object updatedTests) root
            Right (Object updatedRoot)

        _           -> Left "package isnt an Object"

addOneSuite :: Text -> KM.KeyMap Value -> ModuleInfo -> KM.KeyMap Value
addOneSuite projectName tests moduleInfo =
    KM.insert
        (Key.fromText suiteName)
        suiteConfig
        tests
  where
    suiteName = makeSuiteName (moduleName moduleInfo)
    suiteConfig = object
        [ "main"         .= makeMainName (moduleName moduleInfo)
        , "source-dirs"  .= ("test" :: Text)
        , "dependencies" .= (["base", projectName, "hspec"] :: [Text])
        ]

makeSuiteName :: Text -> Text
makeSuiteName name = T.toLower $ T.replace "." "-" name <> "-test"

makeMainName :: Text -> FilePath
makeMainName name = T.unpack $ T.replace "." "" name <> "Main.hs"

savePackage :: Text -> Value -> IO ()
savePackage projectName package = do
    let packagePath = T.unpack projectName </> "package.yaml"
    encodeFile packagePath package