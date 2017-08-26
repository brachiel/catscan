{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( app
    , Api
    , ApiAction
    ) where

import Objects (Category (..))

import Web.Spock
import Web.Spock.Config

import Data.Aeson       hiding (json)
import Data.Monoid      ((<>))
import Data.Text        (Text, pack)
import GHC.Generics

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

app :: Api
app = do
    get "categories" $ do
        json $ [ Category { name = "rechnung" }
               , Category { name = "steuerverwaltung" }
               ]

