{-# LANGUAGE OverloadedStrings #-}

module Repo
    ( config
    
    , Category
    , categories

    , DocumentID
    , documents
    ) where

import Prelude           hiding (lines, readFile) -- use Text instead
import Data.Char         (isAlpha)
import Data.Text         (Text, lines, pack)
import Data.Text.IO      (readFile)
import Data.List         (isSuffixOf)
import Data.List.Split   (splitOn)
import System.Directory  (listDirectory)

import Config

type Category = Text
type DocumentID = FilePath
type DocumentContent = Text

scanCatBasePath = "../" :: FilePath
configFile = scanCatBasePath ++ "config.sh" :: FilePath
keywordsFile = (++) scanCatBasePath <$> config # "KEYWORD_FILE" :: IO FilePath
docBasePath = (++) scanCatBasePath <$> config # "DOCUMENT_DIR" :: IO FilePath
docExt = config # "SCAN_EXT" :: IO String

config :: IO Config
config = do
    ec <- loadConfig configFile
    return $ case ec
             of Left  err    -> error err
                Right config -> config

fromFilePath :: FilePath -> Maybe DocumentID
fromFilePath p = if (all isAlpha base) then Just base
                                       else Nothing
                 where [base, ext] = splitOn "." p

toDocFilePath :: DocumentID -> IO FilePath
toDocFilePath d | all isAlpha d = do
    ext <- docExt
    p <- docBasePath
    return $ p ++ d ++ ext

categories :: IO [Category]
categories = do
    f <- keywordsFile
    t <- readFile f
    return $ lines t

documents :: IO [DocumentID]
documents = do
    p <- docBasePath            :: IO FilePath
    allFiles <- listDirectory p :: IO [FilePath]
    ext <- docExt               :: IO String
    return $ filter (isSuffixOf ext) allFiles

document :: DocumentID -> IO DocumentContent
document doc = do
    p <- toDocFilePath doc
    readFile p

