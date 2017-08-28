{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
    ( app
    , Api
    , ApiAction
    ) where

import qualified Repo

import Prelude                  hiding (id)
import Web.Spock
import Web.Spock.Config

import Data.Aeson               hiding (json)
import Data.Monoid              ((<>))
import Data.Text                (Text, pack)
import GHC.Generics
import Control.Monad.IO.Class   (liftIO)

data Category = Category { name :: Text }
                deriving (Generic, Show)
instance ToJSON Category
instance FromJSON Category

data Document = Document { id :: Repo.DocumentID }
                deriving (Generic, Show)
instance ToJSON Document
instance FromJSON Document

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

app :: Api
app = do
    get "categories" $ do
        cs <- liftIO $ (map fromRepoCategory <$> Repo.categories :: IO [Category]) -- LiftIO lifts IO to ActionT; which can be used with spock
        json cs
    get "documents" $ do
        ds <- liftIO $ (map fromRepoDocumentID <$> Repo.documents :: IO [Document])
        json ds

fromRepoCategory :: Repo.Category -> Category
fromRepoCategory c = Category { name = c }

fromRepoDocumentID :: Repo.DocumentID -> Document
fromRepoDocumentID id = Document { id = id }

