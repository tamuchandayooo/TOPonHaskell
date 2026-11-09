module Infrastructure.ImportAnalyzer where

import Language.Haskell.Exts

extractImports :: FilePath -> IO ([String])
extractImports filePath = do
  result <- parseFile filePath

  case result of
    ParseOk (Module _ _ _ imports _) -> pure (map getModuleName imports)
    ParseOk _                        -> pure []
    ParseFailed location message     -> pure []

getModuleName :: ImportDecl SrcSpanInfo -> String
getModuleName = prettyPrint . importModule