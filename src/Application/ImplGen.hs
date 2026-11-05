module Application.ImplGen where

import System.FilePath (takeFileName, dropExtension)

import Domain.ProjectContext

newModuleImpl :: ProjectContext -> Text -> Text -> FilePath -> IO (FilePath)
newModuleImpl spec structureText testText modulePath = do
    let moduleName  = dropExtension (takeFileName modulePath)
    putStrLn "[INFO]:Generating module:" ++ moduleName ++ " ..."
    -- render prompt 
    let wholeSpec   = (description mSpec) <> "\n" <> (acceptanceTest mSpec)
    let wholePrompt = T.replace "{{project_description}}" wholeSpec     $ 
                      T.replace "{{structure_file}}"      structureText $
                      T.replace "{{module_name}}"         moduleName    $
                      T.replace "{{module_testcode}}"     testText      $
                      mPrompt
    -- generation
    llmGenerateAndSave modulePath wholePrompt ollama (model mSpec)
    return $ acceptanceFilepath tree

    where
        mSpec      = spec context
        cPrompt    = prom context
        mPrompt    = pModule cPrompt

updateModuleImpl :: ProjectContext -> Text -> Text -> FilePath -> Text -> IO (FilePath)
updateModuleImpl spec structureText testText modulePath testResult = do
    let moduleName  = dropExtension (takeFileName modulePath)
    currentFile <- TIO.readFile $ modulePath
    putStrLn "[INFO]:Updating module:" ++ moduleName ++ " ..."
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
    return $ acceptanceFilepath tree

    where
        mSpec      = spec context
        cPrompt    = prom context
        mPrompt    = pModuleRe cPrompt