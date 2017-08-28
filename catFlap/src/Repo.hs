
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

scanCatBasePath = "../"
configFile = scanCatBasePath ++ "config.sh" :: FilePath
keywordsFile = (++) scanCatBasePath <$> config # "KEYWORD_FILE" :: IO FilePath
documentPath = (++) scanCatBasePath <$> config # "DOCUMENT_DIR" :: IO FilePath

config :: IO Config
config = do
    ec <- loadConfig configFile
    return $ case ec
             of Left  err    -> error err
                Right config -> config


categories :: IO [Category]
categories = do
    f <- keywordsFile
    t <- readFile f
    return $ lines t

documents :: IO [DocumentID]
documents = do
    p <- documentPath
    fs <- listDirectory p
    return $ map pack fs

