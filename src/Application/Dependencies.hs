module Application.Dependencies where

import Control.Monad (mapM)
import Data.List (nub)
import Data.Text (Text)

import Domain.ModuleInfo
import Infrastructure.ImportAnalyzer

extractModulesImport :: [ModuleInfo] -> IO ([Text])
extractModulesImport moduleInfo = fmap (concat . nub) $ mapM extractImports implPathes
    where
        implPathes :: [FilePath]
        implPathes = map implPath moduleInfo
        