module Application.Verify where

import System.Exit (exitSuccess)

verify :: IO a -> IO ()
verify f = do
    f
    putStrLn "[INFO]:Are you satisfied with the result? [Y]es, [R]etry, [A]bort:"
    input
    
    where
        input = do
            usrInput <- getLine
            case usrInput of
                "Y" -> return ()
                "R" -> verify f
                "A" -> exitSuccess
                _   -> input 