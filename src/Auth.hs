{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Auth where


import Network.Wai                      (Request, requestHeaders)
import Servant                          (throwError)
import Servant.Server                   (Handler, ServantErr(..),Context ((:.), EmptyContext),
                                         err401, err403, err404, errBody, Server, ServantErr,)
import Servant.API.Experimental.Auth    (AuthProtect)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                         mkAuthHandler)
import Api.User
import Configs                     (App (..), Config (..))
import Auth.Token
import qualified Data.ByteString     as B
import qualified Data.ByteString.Char8     as C

type Auth  = AuthProtect "jwt-auth"
type instance AuthServerData (AuthProtect "jwt-auth") = Token


authHandler :: Config -> AuthHandler Request Token
authHandler cfg = mkAuthHandler (\r ->  authHandler' r)

authHandler' :: Request -> Handler Token
authHandler' req = do
  let handler req' = case lookup "Authorization" (requestHeaders req) of
                       Nothing -> throwError (err401 { errBody = "Missing auth header" })
                       Just authKey -> verifyToken (C.unpack authKey)
   in handler req

verifyToken :: Token -> Handler Token
verifyToken token@"my secret token!" = return token
verifyToken _ = throwError (err401 { errBody = "Invalid Token" })


authServerContext :: Config -> Context (AuthHandler Request Token ': '[])
authServerContext cfg = (authHandler cfg) :. EmptyContext
