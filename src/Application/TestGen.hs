{-# LANGUAGE OverloadedStrings #-}
module Application.TestGen where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Monad (mapM_)

import Domain.ProjectContext
import Domain.StructureTree
import Application.LLMGeneration
import Application.Structure (structureFilename)
import Infrastructure.LLM.Ollama
import Infrastructure.Logger
import GHC.Generics (Datatype(moduleName))

------------------------------------------------------------------------------------------------------

testFilepath :: StructureNode -> FilePath
testFilepath tree = (getProjectname tree) </> "test"
    where
        getProjectname (ProjectNode {name = projectname}) = T.unpack projectname

acceptanceFilepath :: StructureNode -> FilePath
acceptanceFilepath tree = testFilepath tree </> "AcceptanceSpec.hs"

------------------------------------------------------------------------------------------------------

genAcceptance :: ProjectContext -> FilePath -> StructureNode -> IO (FilePath)
genAcceptance context structureFile tree = do
    structureText <- TIO.readFile structureFile
    exist <- doesFileExist $ acceptanceFilepath tree
    if exist
        -- if acceptance test already exists
        then do
            putInfoLog "An acceptance test is already exist. [U]pdate [K]eep [R]egenerate"
            usrInput <- getLine
            case usrInput of
                "U" -> updateAcceptance   context structureText tree
                "K" -> return           $ acceptanceFilepath tree
                "R" -> newAcceptance      context structureText tree
                _   -> genAcceptance      context structureFile tree
        -- if acceptance test isn't exists
        else do
            newAcceptance context structureText tree

newAcceptance :: ProjectContext -> T.Text -> StructureNode -> IO (FilePath)
newAcceptance context structureText tree = do
    putInfoLog "Generating acceptance test ..."
    -- render prompt 
    let wholePrompt = T.replace "{{acceptance_tests}}" acceptance $ 
                      T.replace "{{structure_file}}"   structureText $
                      aPrompt
    -- generation
    llmGenerateAndSave (acceptanceFilepath tree) wholePrompt ollama (model aSpec)
    return $ acceptanceFilepath tree

    where
        aSpec      = spec context
        acceptance = acceptanceTest aSpec
        cPrompt    = prom context
        aPrompt    = pAcceptanceTest cPrompt

updateAcceptance :: ProjectContext -> T.Text -> StructureNode -> IO (FilePath)
updateAcceptance context structureText tree = do
    putInfoLog "Updating acceptance test ..."
    -- render prompt (load current acceptance test)
    currentFile <- TIO.readFile $ acceptanceFilepath tree
    let wholePrompt = T.replace "{{acceptance_tests}}" acceptance $
                      T.replace "{{structure_file}}"   structureText $
                      T.replace "{{current_testcode}}" currentFile $ 
                      aPrompt
    -- generation
    llmGenerateAndSave (acceptanceFilepath tree) wholePrompt ollama (model aSpec)
    return $ acceptanceFilepath tree

    where
        aSpec      = spec context
        acceptance = acceptanceTest aSpec
        cPrompt    = prom context
        aPrompt    = pAcceptanceTestRe cPrompt

------------------------------------------------------------------------------------------------------

genModuleTests :: ProjectContext -> FilePath -> StructureNode -> IO ()
genModuleTests context structureFile tree = do
    let modules = collectModules tree
    structureText <- TIO.readFile structureFile
    mapM_ (genModuleTest context structureText tree) modules

genModuleTest :: ProjectContext -> T.Text -> StructureNode -> T.Text -> IO ()
genModuleTest context structureText tree moduleName =
    if moduleName == "Main"
        then return ()
        else do
            let modulePath = (testFilepath tree) </> ((T.unpack moduleName) ++ "Spec.hs")
            exist <- doesFileExist $ modulePath
            if exist
                -- if module test already exists
                    then do
                        putInfoLog $ "The module " ++ (T.unpack moduleName) ++ "Spec is already exist. [U]pdate [K]eep [R]egenerate"
                        usrInput <- getLine
                        case usrInput of
                            "U" -> updateModuleTest   context structureText tree moduleName
                            "K" -> return             ()
                            "R" -> newModuleTest      context structureText tree moduleName
                            _   -> genModuleTest      context structureText tree moduleName
                            -- if module test isn't exists
                    else newModuleTest context structureText tree moduleName


newModuleTest :: ProjectContext -> T.Text -> StructureNode -> T.Text -> IO ()
newModuleTest context structureText tree moduleName = do
    putInfoLog $ "Generating Test Module:" ++ (T.unpack moduleName) ++ "Spec ..."
    let modulePath = (testFilepath tree) </> ((T.unpack moduleName) ++ "Spec.hs")
    let wholePrompt = T.replace "{{structure_file}}" structureText $
                      T.replace "{{module_name}}"    moduleName    $
                      mPrompt
    llmGenerateAndSave modulePath wholePrompt ollama (model mSpec)
    where
        mSpec   = spec context
        cPrompt = prom context
        mPrompt = pModuleTest cPrompt

updateModuleTest :: ProjectContext -> T.Text -> StructureNode -> T.Text -> IO ()
updateModuleTest context structureText tree moduleName = do
    putInfoLog $ "Updating Test Module:" ++ (T.unpack moduleName) ++ "Spec ..."
    let modulePath = (testFilepath tree) </> ((T.unpack moduleName) ++ "Spec.hs")
    currentFile <- TIO.readFile $ modulePath
    let wholePrompt = T.replace "{{structure_file}}"   structureText $
                      T.replace "{{module_name}}"      moduleName    $
                      T.replace "{{current_testcode}}" currentFile   $
                      mPrompt
    llmGenerateAndSave modulePath wholePrompt ollama (model mSpec)
    where
        mSpec   = spec context
        cPrompt = prom context
        mPrompt = pModuleTestRe cPrompt

collectModules :: StructureNode -> [T.Text]
collectModules tree =
    case tree of
        ModuleNode    n _ _  -> [n]
        ProjectNode   _ _ cs -> concatMap collectModules cs
        DirectoryNode _ _ cs -> concatMap collectModules cs
        _                    -> []