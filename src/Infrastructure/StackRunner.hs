module Infrastructure.StackRunner where

import System.Process
import System.Directory (doesDirectoryExist)
import Control.Exception
import GHC.IO.Exception (ExitCode)

import Domain.TestResult

runStackTest :: FilePath -> IO (Either String TestResult)
runStackTest projectDir = do
    exists <- doesDirectoryExist projectDir
    if not exists
        then return $ Left "There's no Project directory"
        else do
            let command = (proc "stack" ["test"]) { cwd = Just projectDir }
            result <- try (readCreateProcessWithExitCode command "") :: IO (Either IOException (ExitCode, String, String))
            case result of
                Left  err              -> return $ Left (show err)
                Right (code, out, err) -> return $ Right TestResult
                    { testExitCode = code
                    , testStdout = out
                    , testStderr = err
                    }