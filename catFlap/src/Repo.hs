{-# LANGUAGE OverloadedStrings #-}

module Repo
    ( isDocumentID
    , fromFilePath
    , toDocFilePath

    , Category
    , categories

    , DocumentID
    , documents
    , documentPath
    ) where

import Prelude           hiding (lines, readFile) -- use Text instead

import Data.Char         (isAlpha, isDigit)
import Data.List         (isSuffixOf)
import Data.List.Split   (splitOn)
import Data.Maybe        (catMaybes)
import Data.Text         (Text, lines, pack)
import Data.Text.IO      (readFile)
import System.Directory  (listDirectory)

import qualified Config

type Category = Text
type DocumentID = FilePath
type DocumentContent = Text


fromFilePath :: FilePath -> Maybe DocumentID
fromFilePath p = case splitOn "." p of
                    [base, _] | isDocumentID base -> Just base
                    otherwise                    -> Nothing


toDocFilePath :: DocumentID -> IO (Maybe FilePath)
toDocFilePath d | isDocumentID d = do
    ext <- Config.docExt
    p   <- Config.docBasePath
    return . Just $ p ++ '/' : d ++ '.' : ext
toDocFilePath d = return Nothing


isDocumentID :: FilePath -> Bool
isDocumentID = and . map (\c -> isAlpha c || 
                                isDigit c || 
                                c `elem` ("_-()+"::[Char])
                         )

categories :: IO [Category]
categories = do
    f <- Config.keywordsFile
    t <- readFile f
    return $ lines t

documents :: IO [DocumentID]
documents = do
    p        <- Config.docBasePath  :: IO FilePath
    ext      <- Config.docExt       :: IO String
    allFiles <- listDirectory p     :: IO [FilePath]
    return $ catMaybes . map fromFilePath $ filter (isSuffixOf ('.':ext)) allFiles

documentPath :: DocumentID -> IO (Maybe FilePath)
documentPath = toDocFilePath

