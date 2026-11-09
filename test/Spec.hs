import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (withCurrentDirectory, doesFileExist)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>))

import Domain.StructureTree
import Domain.ProjectContext
import Domain.ModuleInfo (getModuleinfoFromTree)
import Application.Verify
import Application.Structure
import Application.TestGen
import Application.ImplGen (genModuleImpls, genAndTestModules)
import Application.Dependencies
import Infrastructure.CreateTemplate
import Infrastructure.ReadYaml
import Application.TestSuite (loadAndSaveTestSuites)

main :: IO ()
main = do
    let root = "./outputs/bibcli"
    treeRes <- parseFromStructureTree structureFilename

    tree <- case treeRes of
        Left  err -> do
            putStrLn $ show err
            exitFailure
        Right str -> pure str

    createFileStructure "." tree
            
    let modules     = getModuleinfoFromTree tree 
    let projectName = T.unpack $ name tree 
    libs <- extractModulesImport modules
    putStrLn $ show modules
    putStrLn $ show libs
