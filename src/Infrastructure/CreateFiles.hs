{-# LANGUAGE OverloadedStrings #-}
module Infrastructure.CreateFiles where

import System.IO (writeFile,appendFile)
import System.IO.Error (tryIOError)
import System.Process (CreateProcess(cwd), createProcess, proc, waitForProcess,std_in,StdStream(..))
import System.Exit (ExitCode(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>),splitDirectories,dropExtension)
import Data.List (intercalate)
import Control.Monad (mapM_)
import Data.Text (Text,unpack)

import Domain.StructureTree

createFile :: FilePath -> StructureNode -> IO (Either String ())
createFile path tree = do
    case tree of
        ProjectNode   _ _ _ -> createProject   path tree
        DirectoryNode _ _ _ -> createDirectory path tree
        ModuleNode    _ _ _ -> createModule    path tree
        TypeNode      _ _   -> createType      path tree
        MethodNode    _ _ _ -> createMethod    path tree

createProject :: FilePath -> StructureNode -> IO (Either String ())
createProject path tree = do
    let fileName = unpack (name tree)
    let dir      = path </> fileName

    result <- tryIOError $ do
        createDirectoryIfMissing True path
        (_, _, _, processHandle) <-
            createProcess $
                (proc "stack" ["new", fileName])
                    { cwd     = Just path 
                    , std_in  = NoStream
                    }
        exitCode <- waitForProcess processHandle
        case exitCode of
            ExitSuccess -> do
                writeFile (dir </> "test" </> "Spec.hs") "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}"
                pure (Right ())
            ExitFailure code ->
                pure $ Left $
                    "stack new failed with exit code " ++ show code
    case result of
        Left ex ->
            pure $ Left $
                show ex ++ " : Failed to create Project named "
                    ++ fileName ++ "!"
        Right _  -> createChildren dir (children tree)

createDirectory :: FilePath -> StructureNode -> IO (Either String ())
createDirectory path tree = do
    let fileName = unpack (name tree)
    let dir      = path </> fileName
    result <- tryIOError $ createDirectoryIfMissing True dir
    case result of
        Left  ex -> pure $ Left $ (show ex) ++ " : " ++ "Failed to create Directory named " ++ fileName ++ "!"
        Right _  -> createChildren dir (children tree)

createModule :: FilePath -> StructureNode -> IO (Either String ())
createModule path tree = do
    let fileName = unpack (name tree)
    let dir      = path </> fileName ++ ".hs"
    let moduleDeclearation = "module " ++ (pathToModule dir) ++ " where\n\n"

    result <- tryIOError $ writeFile dir moduleDeclearation
    case result of
        Left  ex -> pure $ Left $ (show ex) ++ " : " ++ "Failed to create Module named " ++ fileName ++ "!"
        Right _  -> createChildren dir (children tree)
    where
        pathToModule path = intercalate "." $ drop 3 $ splitDirectories $ dropExtension path

createType :: FilePath -> StructureNode -> IO (Either String ())
createType path tree = do
    let typeName = unpack (name tree)
    result <- tryIOError $ appendFile path $ "" --("data " ++ typeName ++ " = undefined\n\n")
    case result of
        Left  ex -> pure $ Left $ (show ex) ++ " : " ++ "Failed to create Type named " ++ typeName ++ "!"
        Right _  -> pure (Right ())

createMethod :: FilePath -> StructureNode -> IO (Either String ())
createMethod path tree = do
    let methodName   = unpack (name tree)
    let signature  = unpack (typeSignature tree)
    result <- tryIOError $ appendFile path $ (methodName ++ " :: " ++ signature ++ "\n" ++ methodName ++ " = undefined\n\n")
    case result of
        Left  ex -> pure $ Left $ (show ex) ++ " : " ++ "Failed to create Method named " ++ methodName ++ "!"
        Right _  -> pure (Right ())
    
createChildren :: FilePath -> [StructureNode] -> IO (Either String ())
createChildren _ [] = pure (Right ())
createChildren dir (child : rest) = do
    result <- createFile dir child
    case result of
        Left err -> pure (Left err)
        Right () -> createChildren dir rest