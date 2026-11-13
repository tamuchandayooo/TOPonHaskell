module Infrastructure.CreateTemplate where

import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>))
import System.Directory (withCurrentDirectory,copyFile,createDirectoryIfMissing,doesFileExist)

import Infrastructure.Logger

createTemplate :: FilePath -> IO ()
createTemplate root = do
    createDirectoryIfMissing True root
    m <- createMain root
    p <- createPrompt root
    if (m || p)
        then exitSuccess
        else return ()

createMain :: FilePath -> IO (Bool)
createMain root = do
    existMain <- doesFileExist $ root </> "main.yaml"
    if not existMain
        then do
            withCurrentDirectory root $ putInfoLog "There aren't \"main.yaml\" in the directory."
            copyFile "main.yaml"   $ root </> "main.yaml"
            withCurrentDirectory root $ putInfoLog "Created the files. Modify \"main.yaml\" and retry."
            return True
        else return False
            
createPrompt :: FilePath -> IO (Bool)
createPrompt root = do    
    existProm <- doesFileExist $ root </> "prompt.yaml"
    if not existProm
        then do
            withCurrentDirectory root $ putInfoLog "There aren't \"prompt.yaml\" in the directory."
            copyFile "prompt.yaml"   $ root </> "prompt.yaml"
            withCurrentDirectory root $ putInfoLog "Created the files. Modify that if needed and retry."
            return True
        else return False