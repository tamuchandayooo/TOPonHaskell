module Application.Verify where

import System.Exit (exitSuccess)

import Infrastructure.Logger

verify :: IO a -> IO ()
verify f = do
    f
    putInfoLog "Are you satisfied with the result? [Y]es, [R]etry, [A]bort:"
    input
    
    where
        input = do
            usrInput <- getLine
            putInputLog usrInput
            case usrInput of
                "Y" -> return ()
                "R" -> verify f
                "A" -> exitSuccess
                _   -> input 