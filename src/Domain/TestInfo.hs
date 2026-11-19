module Domain.TestInfo where

import Data.Text (Text)
import Language.Haskell.Exts

data TestInfo = TestInfo
  { testGroups    :: [Text]
  , testName      :: Text
  , testBody      :: Text
  , testLocation  :: Maybe SrcSpanInfo
  }
  deriving (Show)