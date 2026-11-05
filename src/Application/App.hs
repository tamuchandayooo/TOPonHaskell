module Application.App where

import qualified Data.Text.IO as TIO
import System.Directory (withCurrentDirectory, doesFileExist)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>))

import Domain.ProjectContext
import Application.CreateTemplate
import Application.Verify
import Application.Structure
import Application.TestGen
import Infrastructure.ReadYaml

-- 仕様書とモデルを渡して，全体の処理を実行したい．
-- プロジェクト概要の部分はcontextとしてすべてのLLM生成に引き回す
run :: FilePath -> IO ()
run root = do
    
    -- Create template files if there aren't main.yaml or prompt.yaml 
    createTemplate root

    -- Set argument path as current directory
    withCurrentDirectory root $ do



        -------- initalization --------

        context <- do
            -- Read specification from external file in yaml
            -- input  : yaml file
            -- output : Specs (ADT)
            rSpec <- readSpec $ "main.yaml"
            -- Read prompts from external file in yaml
            -- input  : yaml file
            -- output : Prompts (ADT)
            rProm <- readProm $ "prompt.yaml"

            -- Define project context every method use
            pure $ case (rSpec, rProm) of
                (Right spec, Right prom) -> Right ProjectContext
                    { spec = spec
                    , prom = prom
                    }
                (Left err, _)            -> Left  err
                (_, Left err)            -> Left  err



        -------- structure generation --------

        -- Generate structure in yaml by LLM
        -- input  : Context (ADT)
        -- output : yaml file
        case context of
            Left  err -> do
                putStrLn $ show err
                exitFailure
            Right cxt -> do
                verify $ genStructure cxt
    
        -- Create structure tree
        -- input  : structure file
        -- output : StructureTree (ADT)
        tree <- parseFromStructureTree structureFilename

        -- Create stack project and file structure
        -- input  : StructureTree (ADT)
        -- output : stack project (IO ())
        case tree of
            Left  err -> do
                putStrLn $ show err
                exitFailure
            Right tre -> createFileStructure "." tre
                 

        
        -------- test generations --------

        case tree of
            Left  err -> do
                putStrLn $ show err
                exitFailure
            Right tre -> do
                case context of
                    Left  err2 -> do
                        putStrLn $ show err2
                        exitFailure
                    Right cxt  -> do
                        -- Generate acceptance test by LLM
                        -- input  : Context (ADT)
                        -- output : hspec test
                        verify $ genAcceptance cxt structureFilename tre

                        -- Generate module test by LLM
                        -- input  : Context (ADT), Text, StructureTree (ADT)
                        -- output : hspec test
                        verify $ genModuleTests cxt structureFilename tre
    

        
        

        {-

        -------- Iterating Implementation

        genAndRunImpl spec structureFile tree acceptance prom
        {-
            -- 〇iteration
            -- Generate main module by LLM
            -- input  : Context (ADT), yaml file, hspec test
            -- output : 
            genMain context structureFile acceptance

            -- Generate each module by LLM
            -- input  : Context (ADT), yaml file, StructureTree (ADT)
            -- output : 
            genModule context structureFile tree

            -- Run test for implementations
            runTest
        -}
        -}

{-

Specs
    { model
    , description
    , acceptanceTest
    }

Prompts
    { pStructure
    , pAcceptanceTest
    , pModuleTest
    , pMain
    , pModule
    }

-}