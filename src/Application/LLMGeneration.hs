{-# LANGUAGE OverloadedStrings #-}
module Application.LLMGeneration where

import Data.Text(Text,pack)

import Domain.LLMRequest
import Infrastructure.FileOutput
import Infrastructure.Logger

------------------------------------------------------------------------------------------------------

data LLMRepository m = LLMRepository
    { request  :: LLMRequest -> m (Either String LLMResponse)
    }
{-
    ollama
-}

llmRequest :: LLMRequest -> LLMRepository m -> m (Either String LLMResponse)
llmRequest req repo = do
    request repo req

llmGenerate :: (Monad m) => Text -> LLMRepository m -> Text -> m (Either String Text)
llmGenerate text repo model = do
    let req = createRequestFromText text model
    response <- request repo req
    case response of
        Left  err -> pure $ Left $ show err -- pending．なんらかのハンドリングをしたい
        Right res -> do
            let mes = message res
            pure $ Right $ content mes

llmGenerateAndSave :: FilePath -> Text -> LLMRepository IO -> Text -> IO ()
llmGenerateAndSave path text repo model = do
    response <- llmGenerate text repo model
    case response of
        Left  err -> putErrorLog $ show err
        Right res -> fileOutput path res
