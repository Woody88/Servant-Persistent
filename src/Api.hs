{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api (app) where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Utils.Enter         (runReaderTNat)
import           Configs                     (App (..), Config (..))
import           Models
import           Control.Category     ((<<<), (>>>))
import           Api.User
import           Auth
import           Auth.Token

-- -- | This is the function we export to run our 'UserAPI'. Given
-- -- a 'Config', we return a WAI 'Application' which any WAI compliant server
-- -- can run.
-- userApp :: Config -> Token -> Application
-- userApp cfg t = serve (Proxy :: Proxy UserAPI) (appToServer cfg)
--
-- -- | This functions tells Servant how to run the 'App' monad with our
-- -- 'server' function.
appToUserServer :: Config -> Token -> Server UserAPI
appToUserServer cfg t = enter (convertApp cfg ) (userServer t)

appToLoginServer :: Config -> Server LoginAPI
appToLoginServer cfg = enter (convertApp cfg ) loginServer

-- | This function converts our 'App' monad into the @ExceptT ServantErr
-- IO@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
convertApp :: Config -> App :~> Handler
convertApp cfg = runReaderTNat cfg <<< NT runApp

-- env r = runReaderT r env
--
-- -- | Since we also want to provide a minimal front end, we need to give
-- -- Servant a way to serve a directory with HTML and JavaScript. This
-- -- function creates a WAI application that just serves the files out of the
-- -- given directory.
files :: Server Raw
files = serveDirectoryFileServer "assets"
--
-- -- | Just like a normal API type, we can use the ':<|>' combinator to unify
-- -- two different APIs and applications. This is a powerful tool for code
-- -- reuse and abstraction! We need to put the 'Raw' endpoint last, since it
-- -- always succeeds.
type AppAPI = Auth :> UserAPI :<|> LoginAPI :<|> Raw

appApi :: Proxy AppAPI
appApi = Proxy

serverApi :: Config -> Server AppAPI
serverApi cfg = (appToUserServer cfg :<|> appToLoginServer cfg :<|> files)

-- -- | Finally, this function takes a configuration and runs our 'UserAPI'
-- -- alongside the 'Raw' endpoint that serves all of our files.
app :: Config -> Application
app cfg =
    serveWithContext appApi (authServerContext cfg) (serverApi cfg)
