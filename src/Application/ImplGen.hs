{-# LANGUAGE OverloadedStrings #-}
module Application.ImplGen where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO 
import System.FilePath (takeFileName, dropExtension,(</>))

import qualified Domain.StructureTree as ST
import Domain.ProjectContext
import Domain.ModuleInfo
import Application.LLMGeneration
import Infrastructure.LLM.Ollama
import Application.TestGen (testFilepath)

genModuleImpls :: ProjectContext -> FilePath -> T.Text -> [ModuleInfo] -> IO ()
genModuleImpls context structureFile testResult modules = do
    structureText <- TIO.readFile structureFile
    mapM_ (genModuleImpl context structureText testResult) modules

genModuleImpl :: ProjectContext -> T.Text -> T.Text -> ModuleInfo -> IO (FilePath)
genModuleImpl context structureText testResult moduleInfo =
    if null (T.unpack testResult)
        then newModuleImpl    context structureText            moduleInfo
        else updateModuleImpl context structureText testResult moduleInfo

newModuleImpl :: ProjectContext -> T.Text -> ModuleInfo -> IO (FilePath)
newModuleImpl context structureText moduleInfo = do
    let moduleName  = T.pack $ dropExtension (takeFileName (implPath moduleInfo))
    let modulePath  = implPath moduleInfo
    testText <- TIO.readFile (testPath moduleInfo)
    putStrLn $ "[INFO]:Generating module:" ++ (T.unpack moduleName) ++ " ..."
    -- render prompt 
    let wholeSpec   = (description mSpec) <> "\n" <> (acceptanceTest mSpec)
    let wholePrompt = T.replace "{{project_description}}" wholeSpec           $ 
                      T.replace "{{structure_file}}"      structureText       $
                      T.replace "{{module_name}}"         moduleName          $
                      T.replace "{{module_testcode}}"     testText            $
                      mPrompt
    -- generation
    llmGenerateAndSave modulePath wholePrompt ollama (model mSpec)
    return $ modulePath

    where
        mSpec      = spec context
        cPrompt    = prom context
        mPrompt    = pModule cPrompt

updateModuleImpl :: ProjectContext -> T.Text -> T.Text -> ModuleInfo -> IO (FilePath)
updateModuleImpl context structureText testResult moduleInfo = do
    let moduleName  = T.pack $ dropExtension (takeFileName (implPath moduleInfo))
    let modulePath  = implPath moduleInfo
    testText    <- TIO.readFile (testPath moduleInfo)
    currentFile <- TIO.readFile (implPath moduleInfo)
    putStrLn $ "[INFO]:Updating module:" ++ (T.unpack moduleName) ++ " ..."
    -- render prompt 
    let wholeSpec   = (description mSpec) <> "\n" <> (acceptanceTest mSpec)
    let wholePrompt = T.replace "{{project_description}}"    wholeSpec     $ 
                      T.replace "{{structure_file}}"         structureText $
                      T.replace "{{module_name}}"            moduleName    $
                      T.replace "{{module_testcode}}"        testText      $
                      T.replace "{{current_implementation}}" currentFile   $
                      T.replace "{{test_results}}"           testResult    $
                      mPrompt
    -- generation
    llmGenerateAndSave modulePath wholePrompt ollama (model mSpec)
    return $ modulePath

    where
        mSpec      = spec context
        cPrompt    = prom context
        mPrompt    = pModule cPrompt
