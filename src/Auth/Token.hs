{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}


module Auth.Token where
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics                        (Generic)
import qualified Data.ByteString     as B

type Token = String

tokenKey :: Token
tokenKey = "my secret token!"
