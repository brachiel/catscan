{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
    ( app
    , Api
    , ApiAction
    ) where

import qualified Repo
import qualified Config

import Prelude                  hiding (id)
import Web.Spock
import Web.Spock.Config

import Data.Aeson               hiding (json)
import Data.Monoid              ((<>))
import Data.Text                (Text, pack, append)
import Data.Text.Encoding       (encodeUtf8)
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
    get root $ file "text/html" "static/index.html"
    get ("static/js" <//> var) $ \f -> file "text/javascript" $ "static/js/" ++ f
    get ("static/css/images" <//> var) $ \f -> file "image/png" $ "static/css/images/" ++ f
    get ("static/css/images/icons-svg" <//> var) $ \f -> file "image/svg" $ "static/css/images/icons-svg/" ++ f
    get ("static/css/images/icons-png" <//> var) $ \f -> file "image/png" $ "static/css/images/icons-png/" ++ f
    get ("static/css" <//> var) $ \f -> file "text/css" $ "static/css/" ++ f
    get "categories" $ do
        cs <- liftIO $ (map fromRepoCategory <$> Repo.categories :: IO [Category]) -- LiftIO lifts IO to ActionT; which can be used with spock
        json cs
    get "documents" $ do
        ds <- liftIO $ (map fromRepoDocumentID <$> Repo.documents :: IO [Document])
        json ds
    get ("document" <//> var) $ \dId -> do
        path <- liftIO $ Repo.documentPath dId
        ext <- liftIO Config.docExt
        case path of
            Just doc -> file ("image/" `append` pack ext) doc
            Nothing  -> text "error"

fromRepoCategory :: Repo.Category -> Category
fromRepoCategory c = Category { name = c }

fromRepoDocumentID :: Repo.DocumentID -> Document
fromRepoDocumentID id = Document { id = id }

