module Infrastructure.ImportAnalyzer where

import Language.Haskell.Exts
import qualified Data.Text as T (Text,pack)
import System.Process
import System.Exit

extractImports :: FilePath -> IO ([T.Text])
extractImports filePath = do
  result <- parseFile filePath

  case result of
    ParseOk (Module _ _ _ imports _) -> pure (map getModuleName imports)
    ParseOk _                        -> pure []
    ParseFailed _ _                  -> pure []

getModuleName :: ImportDecl SrcSpanInfo -> T.Text
getModuleName decl = T.pack $ prettyPrint . importModule $ decl