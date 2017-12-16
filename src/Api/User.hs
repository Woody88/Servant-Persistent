{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Api.User where

import           GHC.Generics (Generic)
import           Data.Aeson                          as A
import           Data.Aeson.TH
import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Configs                      (App (..), Config (..))
import           Models
import           Auth.Token                         (Token, tokenKey)


data LoginUser = Login
    { email    :: String
    , password :: String
    } deriving (Generic, Show, Read)

$(deriveJSON defaultOptions ''LoginUser)

type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "name" String :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] User  :> Post '[JSON] Int64

type LoginAPI = "login" :> ReqBody '[JSON] LoginUser :> Post '[JSON] Token


-- | The server that runs the UserAPI
userServer :: Token -> ServerT UserAPI App
userServer t = allUsers :<|> singleUser :<|> createUser

loginServer :: ServerT LoginAPI App
loginServer = loginUser

loginUser :: LoginUser -> App Token
loginUser creds = return tokenKey


-- | Returns all users in the database.
allUsers :: App [Entity User]
allUsers =
    runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleUser :: String -> App (Entity User)
singleUser str = do
    maybeUser <- runDb (selectFirst [UserName ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just person ->
            return person

-- | Creates a user in the database.
createUser :: User -> App Int64
createUser p = do
    newUser <- runDb (insert (User (userName p) (userEmail p)))
    return $ fromSqlKey newUser

-- | Generates JavaScript to query the User API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"
