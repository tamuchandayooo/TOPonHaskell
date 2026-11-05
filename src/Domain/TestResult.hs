module Domain.TestResult where

import GHC.IO.Exception (ExitCode)

data TestResult = TestResult
    { testExitCode :: ExitCode
    , testStdout   :: String
    , testStderr   :: String
    }