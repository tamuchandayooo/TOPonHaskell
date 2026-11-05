module Application.TestSuite where

import Data.Text (Text)

import Infrastructure.PackageYaml
import Domain.StructureTree
import Domain.ModuleInfo (ModuleInfo)
import System.Exit (exitFailure)

loadAndSaveTestSuites :: StructureNode -> [ModuleInfo] -> IO ()
loadAndSaveTestSuites tree moduleInfo = do
    package <- loadPackage tree
    let result = addTestSuites (name tree) package moduleInfo
    case result of
        Left  err -> exitFailure
        Right val -> savePackage (name tree) val
