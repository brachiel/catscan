
module Repo
    ( config
    
    , Category
    , categories

    , DocumentID
    , documents
    ) where

import Prelude          hiding (lines, readFile) -- use Text instead
import Data.Text        (Text, lines, pack)
import Data.Text.IO     (readFile)
import System.Directory (listDirectory)

import Config

type Category = Text
type DocumentID = Text

configFile = "../config.sh"
keywordsFile = "../keywords"
documentPath = "../documents"

config :: IO Config
config = do
    ec <- loadConfig configFile
    return $ case ec
             of Left  err    -> error err
                Right config -> config


categories :: IO [Category]
categories = lines <$> readFile keywordsFile

documents :: IO [DocumentID]
documents = fmap pack <$> listDirectory documentPath

