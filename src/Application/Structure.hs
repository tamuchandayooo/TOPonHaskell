{-# LANGUAGE OverloadedStrings #-}
module Application.Structure where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)

import Domain.ProjectContext
import qualified Domain.StructureTree as ST
import Application.LLMGeneration
import Infrastructure.LLM.Ollama
import Infrastructure.ReadYaml
import Infrastructure.CreateFiles
import Infrastructure.Logger

------------------------------------------------------------------------------------------------------

structureFilename :: FilePath
structureFilename = "structure.yaml"

genStructure :: ProjectContext -> IO (FilePath)
genStructure context = do
    exist <- doesFileExist structureFilename
    if exist
        -- if structure file already exists
        then do
            putInfoLog "A structure file is already exist. [U]pdate [K]eep [R]egenerate"
            usrInput <- getLine
            putInputLog usrInput
            case usrInput of
                "U" -> updateStructure context
                "K" -> return structureFilename
                "R" -> newStructure context
                _   -> genStructure context
        -- if structure file isn't exists
        else do
            newStructure context
            

newStructure :: ProjectContext -> IO (FilePath)
newStructure context = do
    putInfoLog "Generating structure file ..."
    -- render prompt
    let wholeSpec   = (description sSpec) <> "\n" <> (acceptanceTest sSpec)
    let wholePrompt = T.replace "{{project_description}}" wholeSpec sPrompt
    -- generation
    llmGenerateAndSave structureFilename wholePrompt ollama (model sSpec)
    return structureFilename

    where
        sSpec       = spec context
        cPrompt     = prom context
        sPrompt     = pStructure cPrompt

updateStructure :: ProjectContext -> IO (FilePath)
updateStructure context = do
    putInfoLog "Updating structure file ..."
    -- render prompt (load current structure yaml)
    currentFile <- TIO.readFile structureFilename
    let wholeSpec   = (description sSpec) <> "\n" <> (acceptanceTest sSpec)
    let wholePrompt = T.replace "{{project_description}}" wholeSpec $ T.replace "{{current_structure}}" currentFile sPrompt
    -- generation
    llmGenerateAndSave structureFilename wholePrompt ollama (model sSpec)
    return structureFilename

    where
        sSpec       = spec context
        cPrompt     = prom context
        sPrompt     = pStructureRe cPrompt

------------------------------------------------------------------------------------------------------

parseFromStructureTree :: FilePath -> IO (Either String ST.StructureNode)
parseFromStructureTree path = do
    read <- readStructure path
    case read of
        Left  err -> pure $ Left  $ show err
        Right tre -> pure $ Right $ tre

createFileStructure :: FilePath -> ST.StructureNode -> IO ()
createFileStructure path tree = do
    putInfoLog "Creating File Structure ..."
    crtFile <- createFile path tree
    case crtFile of
        Left  err -> putStrLn $ show err
        Right res -> return ()