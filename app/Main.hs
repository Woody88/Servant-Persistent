module Main where

import Database.Persist.Postgresql (runSqlPool, runSqlPersistMPool)
import Network.Wai.Handler.Warp    (run)
import System.Environment          (lookupEnv)
import Database.Persist.Postgresql
import Api                         (app)
import Api.User                    (generateJavaScript)
import Configs
import Models                      (runDBMigrations, doMigrations)
import Safe                        (readMay)
import Control.Monad.IO.Class      (liftIO)


main :: IO ()
main = do
  putStrLn "servant-persistent booting up"
  env  <- lookupSetting "ENV" Development
  port <- lookupSetting "PORT" 8080
  pool <- makePool env
  let cfg    = Config { getPool = pool, getEnv = env, getPort = port }
      logger = setLogger env
  runSqlPersistMPool doMigrations pool
  generateJavaScript
  putStrLn $ "Running server on port: " ++ (show port)
  run port $ logger $ app cfg
  where readPort p = read p :: Int

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing ->
      return def
    Just str ->
      maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]
