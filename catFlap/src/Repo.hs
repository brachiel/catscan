{-# LANGUAGE OverloadedStrings #-}

module Repo
    ( isDocumentID
    , fromFilePath
    , toDocFilePath

    , Category (..)
    , categories
    , categoryDocuments

    , DocumentID
    , documents
    , documentPath
    , documentCategories
    ) where

import Prelude           hiding (lines, readFile) -- use Text instead

import Data.Char         (isAlpha, isDigit)
import Data.List         (isSuffixOf)
import Data.List.Split   (splitOn)
import qualified Data.Map as Map
import Data.Maybe        (fromMaybe, catMaybes)
import Data.Text         (Text, lines, pack, unpack)
import Data.Text.IO      (readFile)
import System.Directory  (listDirectory)

import qualified Config

type DocumentID = FilePath
type DocumentContent = Text

data Category = Category Text
data CategoryMap = CategoryMap (Map.Map Category [DocumentID])


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


toDocCategoryFilePath :: DocumentID -> IO (Maybe FilePath)
toDocCategoryFilePath d = do
    ext <- Config.catExt
    p   <- toDocFilePath d :: IO (Maybe FilePath)
    return $ fmap (++ '.' : ext) p


isDocumentID :: FilePath -> Bool
isDocumentID = and . map (\c -> isAlpha c || 
                                isDigit c || 
                                c `elem` ("_-()+"::[Char])
                         )

readLines :: FilePath -> IO [Text]
readLines = fmap lines . readFile

readCategories :: FilePath -> IO [Category]
readCategories = fmap (map Category) . readLines

categories :: IO [Category]
categories = Config.keywordsFile >>= readCategories

documents :: IO [DocumentID]
documents = do
    p        <- Config.docBasePath  :: IO FilePath
    ext      <- Config.docExt       :: IO String
    allFiles <- listDirectory p     :: IO [FilePath]
    return $ catMaybes . map fromFilePath $ filter (isSuffixOf ('.':ext)) allFiles

documentPath :: DocumentID -> IO (Maybe FilePath)
documentPath = toDocFilePath

-- TODO: Filter and only let actual categories pass
documentCategories :: DocumentID -> IO [Category]
documentCategories d = do
    path      <- toDocCategoryFilePath d     :: IO (Maybe FilePath)
    let maybeCats = fmap readCategories path :: Maybe (IO [Category])
    fromMaybe (return []) maybeCats

-- |Documents per category are stored in files in the category directory
categoryDocuments :: Category -> IO [DocumentID]
categoryDocuments (Category category) = do
    path      <- Config.catBasePath
    fmap (catMaybes . map (fromFilePath . unpack)) $ readLines $ path ++ '/' : (unpack category)

