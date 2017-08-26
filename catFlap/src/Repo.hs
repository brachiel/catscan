
module Repo
    ( Category
    , categories

    , DocumentID
    , documents
    ) where

import Prelude          hiding (lines, readFile) -- use Text instead
import Data.Text        (Text, lines, pack)
import Data.Text.IO     (readFile)
import System.Directory (listDirectory)

type Category = Text
type DocumentID = Text

keywordsFile = "../../keywords"
documentPath = "../../documents"

categories :: IO [Category]
categories = lines <$> readFile keywordsFile

documents :: IO [DocumentID]
documents = fmap pack <$> listDirectory documentPath

