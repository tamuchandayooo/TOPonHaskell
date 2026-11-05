module Domain.ProjectContext where

import Data.Text (Text)
import System.Exit (exitFailure)

data ProjectContext = ProjectContext
    { spec :: Specs
    , prom :: Prompts
    } deriving (Show)

data Specs = Specs
    { model          :: Text
    , description    :: Text
    , acceptanceTest :: Text
    } deriving (Show)

-- どうせ全部のプロンプトにRe(すでにある場合の更新用)があるんだったら，リストとかでバリアントを管理すべきだったかも．とりあえずpending
data Prompts = Prompts
    { pStructure        :: Text
    , pStructureRe      :: Text
    , pAcceptanceTest   :: Text
    , pAcceptanceTestRe :: Text
    , pModuleTest       :: Text
    , pModuleTestRe     :: Text
    , pMain             :: Text
    , pMainRe           :: Text
    , pModule           :: Text
    , pModuleRe         :: Text
    } deriving (Show)


createContext rSpec rProm = do
    case (rSpec, rProm) of
        (Right spec, Right prom) -> Right ProjectContext
            { spec = spec
            , prom = prom
            }
        (Left err, _)            -> Left  err
        (_, Left err)            -> Left  err