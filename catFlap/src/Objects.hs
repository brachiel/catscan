{-# LANGUAGE DeriveGeneric #-}

module Objects
    ( Category (..)
    ) where

import Data.Aeson       hiding (json)
import Data.Monoid      ((<>))
import Data.Text        (Text, pack)
import GHC.Generics

data Category = Category { name :: Text }
                deriving (Generic, Show)
instance ToJSON Category
instance FromJSON Category
