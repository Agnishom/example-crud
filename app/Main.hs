{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Servant
import Data.Aeson
import Data.Aeson.Casing
import Data.Pool (createPool, withResource)
import GHC.Generics
import Opaleye
import Database.PostgreSQL.Simple
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Arrow
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Network.Wai.Handler.Warp
import qualified GHC.Int

type CrudAPI = "read" :> Get '[JSON] [User]
                :<|> "read" :> Capture "name" String :> Get '[JSON] [User]
                :<|> "create" :> ReqBody '[JSON] User :> Post '[JSON] [GHC.Int.Int64]
                :<|> "update" :> ReqBody '[JSON] User :> Post '[JSON] [GHC.Int.Int64]
                :<|> "delete" :> ReqBody '[PlainText] String :> Post '[JSON] [GHC.Int.Int64]

data UserPoly name city age = User
  { userName :: name
  , userCity :: city
  , userAge :: age
  } deriving (Eq, Show, Generic)

type User = UserPoly String String Int


$(makeAdaptorAndInstance "pUser" ''UserPoly)
$(makeLensesWith abbreviatedFields ''UserPoly)

userTable :: Table
            (UserPoly (Column PGText) (Column PGText) (Column PGInt4))
            (UserPoly (Column PGText) (Column PGText) (Column PGInt4))
userTable = Table "users" (pUser User { userName = required "name",
                                        userCity = required "city",
                                        userAge = required "age"})

instance FromJSON User
  where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToJSON User
  where
  toJSON  = genericToJSON $ aesonPrefix camelCase
  toEncoding = genericToEncoding $ aesonPrefix camelCase

crudApi :: Connection -> Server CrudAPI
crudApi conn = enter (NT $ liftIO . (`runReaderT` conn)) crudApiT

crudApiT :: ServerT CrudAPI (ReaderT Connection IO)
crudApiT = readAPI1 :<|> readAPI2 :<|> createAPI :<|> updateAPI :<|> deleteAPI

readAPI1 :: ReaderT Connection IO [User]
readAPI1 = do
              conn <- ask
              users <- liftIO $ runQuery conn $ queryTable userTable
              return users

readAPI2 :: String -> ReaderT Connection IO [User]
readAPI2 ns = do
              conn <- ask
              users <- liftIO $ runQuery conn (nameQuery ns)
              return users

createAPI :: User -> ReaderT Connection IO [GHC.Int.Int64]
createAPI u = do
              conn <- ask
              success <- liftIO $ runInsert conn userTable (User (pgString (userName u)) (pgString (userCity u)) (pgInt4 (userAge u)))
              return [success]

updateAPI :: User -> ReaderT Connection IO [GHC.Int.Int64]
updateAPI u = do
              conn <- ask
              success <- liftIO $ runUpdate conn userTable (const $ User (pgString (userName u)) (pgString (userCity u)) (pgInt4 (userAge u)))
                                                  (\ entry -> userName entry .== pgString (userName u))
              return [success]

deleteAPI :: String -> ReaderT Connection IO [GHC.Int.Int64]
deleteAPI n = do
              conn <- ask
              success <- liftIO $ runDelete conn userTable (\entry -> userName entry .== pgString n)
              return [success]

nameQuery :: String -> Opaleye.Query (UserPoly (Column PGText) (Column PGText) (Column PGInt4))
nameQuery ns = proc () -> do
                  row <- (queryTable userTable) -< ()
                  restrict -< (userName row .== pgString ns)
                  returnA -< row

connectionInfo :: ConnectInfo
connectionInfo = ConnectInfo{connectHost="localhost"
                             ,connectPort=5432
                             ,connectDatabase="mydb"
                             ,connectPassword="b2b"
                             ,connectUser="b2b"
                             }

main :: IO ()
main = do
        pool <- createPool (connect connectionInfo) close 1 10 5
        withResource pool $ \conn ->
                            run 8081 (serve (Proxy :: Proxy CrudAPI) (crudApi conn))

--data  = MyData [(String, [(String, String)])]
--instance ToJSON MyData