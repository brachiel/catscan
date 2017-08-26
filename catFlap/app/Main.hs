
module Main where

-- Based on https://www.spock.li/tutorials/rest-api

import qualified Api

import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 (spock spockCfg Api.app)

