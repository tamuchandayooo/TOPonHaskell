module Application.App where

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
import Infrastructure.PackageYaml

-- 仕様書とモデルを渡して，全体の処理を実行したい．
-- プロジェクト概要の部分はcontextとしてすべてのLLM生成に引き回す
run :: FilePath -> IO ()
run root = do
    
    -- Create template files if there aren't main.yaml or prompt.yaml 
    createTemplate root

    -- Set argument path as current directory
    withCurrentDirectory root $ do



        -------- initalization --------

        
        -- Read specification from external file in yaml
        -- input  : yaml file
        -- output : Specs (ADT)
        rSpec <- readSpec $ "main.yaml"
        -- Read prompts from external file in yaml
        -- input  : yaml file
        -- output : Prompts (ADT)
        rProm <- readProm $ "prompt.yaml"

        -- Define project context every method use
        context <- case createContext rSpec rProm of
            Left err  -> do
                putStrLn $ show err
                exitFailure
            Right cxt -> pure cxt



        -------- structure generation --------

        -- Generate structure in yaml by LLM
        -- input  : Context (ADT)
        -- output : yaml file
        verify $ genStructure context
    
        -- Create structure tree
        -- input  : structure file
        -- output : StructureTree (ADT)
        treeRes <- parseFromStructureTree structureFilename

        tree <- case treeRes of
            Left  err -> do
                putStrLn $ show err
                exitFailure
            Right str -> pure str

        -- Create stack project and file structure
        -- input  : StructureTree (ADT)
        -- output : stack project (IO ())
        createFileStructure "." tree
                 

        
        -------- test generations --------

        
        -- Generate acceptance test by LLM
        -- input  : Context (ADT)
        -- output : hspec test
        verify $ genAcceptance context structureFilename tree

        -- Generate module test by LLM
        -- input  : Context (ADT), Text, StructureTree (ADT)
        -- output : hspec test
        verify $ genModuleTests context structureFilename tree 

        

        -------- Iterating Implementation --------
        let modules     = getModuleinfoFromTree tree 
        let projectName = T.unpack $ name tree 
        putStrLn $ show modules

        genAndTestModules context structureFilename projectName (T.pack "") modules
        -- genModuleImpls context structureFilename (T.pack "") modules