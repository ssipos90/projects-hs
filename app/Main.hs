{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Database.Redis (Connection, checkedConnect, defaultConnectInfo, runRedis)
import Lib
import System.IO (IO (..))
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (mkStatus, status404)
import Protolude hiding (get, put)
import Prelude (read)
import Control.Monad.State 
import Lens.Micro (over, (^.))
import Lens.Micro.TH (makeLensesFor)

data Task = Task { id:: Int
                 , title :: Text
                 , done :: Bool
                 } deriving (Show, Generic)

data AppState = AppState { tasks :: [Task]
                         , conn :: Connection
                         } 
instance FromJSON Task
instance ToJSON Task

makeLensesFor [("tasks", "_tasks")] ''AppState

--newtype MyState a = StateT AppState IO () a

main :: IO ()
main = do
    conn <- checkedConnect defaultConnectInfo
    runStateT serve AppState{ tasks = [], conn = conn }
    return ()

serve :: StateT AppState IO ()
serve = do
    liftIO $ S.scotty 3000 $ do
        S.get "/" $ do
          S.json ("This was a GET  request!" :: Text)
        S.get "/tasks" $ do
          appState <- get
          S.json ( (appState ^. _tasks) :: [Task])
        S.get "/tasks/:taskId" $ do
          appState <- get
          taskId <- (\s -> read s :: Int). T.unpack <$> S.param "taskId"
          void $ case find (\Task { id = id } -> id == taskId) (appState ^. _tasks) of
            Just task -> S.json task
            Nothing -> do
              S.status status404
              S.json (404 :: Int)
        --S.delete "/tasks/:taskId" $ do
        --    S.json ("This was a DELETE request!" :: Text)
        --S.post "/tasks" $ do
        --    S.json ("This was a POST request!" :: Text)
        --S.put "/tasks/:taskId" $ do
        --    S.json ("This was a PUT request!" :: Text)
    return ()

