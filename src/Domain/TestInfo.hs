module Domain.TestInfo where

import Data.Text (Text)
import Language.Haskell.Exts

data TestInfo = TestInfo
  { testGroups    :: [Text]
  , testName      :: Text
  , input         :: Text
  , expected      :: Text
  , assertion     :: Text
  , testStartLine :: Int
  , testEndLine   :: Int
  }
  deriving (Show)

data HspecStatement l
  = Describe String (Exp l)
  | Context  String (Exp l)
  | It       String (Exp l)
  | Specify  String (Exp l)
  deriving (Show)