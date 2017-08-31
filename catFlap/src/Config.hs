{- |
Module      :  $Header$
Description :  Parser of catscan config
Copyright   :  (c) brachiel@github
License     :  All rights reserved.

Maintainer  :  brachiel@github
Stability   :  unstable
Portability :  portable

Reads and parses the catscan configuration file and exposes the settings
in neatly IO wrapped values. Fails miserably in case of parsing errors.
-}

module Config
    ( Config (..)
    , config
    , loadConfig

    , catScanBasePath
    , configFile
    , keywordsFile
    , docExt
    , docBasePath
    , catExt
    , catBasePath
    ) where

-- Based on http://vaibhavsagar.com/blog/2017/08/13/i-haskell-a-git/
-- and http://www.serpentine.com/blog/2007/01/31/parsing-a-simple-config-file-in-haskell/

import Prelude hiding (takeWhile, lines, readFile)
import Data.Attoparsec.Text
import Data.Char (isAlpha, isSpace)
import Data.Text (Text, pack, unpack, lines)
import Data.Text.IO (readFile)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import Control.Applicative ((<|>))
import Control.Monad (sequence)


-- Config

-- |'catScanBasePath' is the base path for all other path based options.
-- It must point to the config.sh file of catscan.
catScanBasePath = "../" :: FilePath

-- |'configFile' points to the config.sh configuration file of catscan.
configFile = catScanBasePath ++ "config.sh" :: FilePath

-- |'keywordsFile' points to the keywords file of catscan containing
-- a list of keywords that catscan knows. Its location is read from
-- the config file and is thus wrapped in IO
keywordsFile = (++) catScanBasePath <$> config # "KEYWORD_FILE" :: IO FilePath

-- |The 'docBasePath' points to location of the raw repository, i.e.
-- to the location where the scanned files and their meta files are.
-- It is read from the config file and is thus wrapped in IO
docBasePath = (++) catScanBasePath <$> config # "DOCUMENT_DIR" :: IO FilePath

-- |The 'docExt' is the extension used for documents in 'docBasePath'.
-- It is read from the config file and is thus wrapped in IO
docExt = fmap tail $ config # "SCAN_EXT" :: IO String

-- |The 'catExt' is the extension used for the category meta files in 'docBasePath'.
-- It is read from the config file and is thus wrapped in IO
catExt = fmap tail $ config # "CAT_EXT" :: IO String

-- |The 'catBasePath' points to location of the category map files.
-- Each of the map files contains a list with document names.
-- It is read from the config file and is thus wrapped in IO
catBasePath = (++) catScanBasePath <$> config # "CATEGORY_DIR" :: IO FilePath


data Config = Config (Map.Map Text Text)

config :: IO Config
config = do
    ec <- loadConfig configFile
    return $ case ec
             of Left  err    -> error err
                Right config -> config


-- |Read key from (IO Config)
(#) :: IO Config -> String -> IO String
config # key = do
    Config cfg <- config
    return $ unpack $ cfg Map.! (pack key)

identifier :: Parser Text
identifier = do
    c  <- letter <|> char '_'
    cs <- many' $ letter <|> digit <|> char '_'
    return $ pack (c:cs)
    <?> "identifier"

comment :: Parser ()
comment = do
            char '#'
            skipWhile (const True)
           <?> "comment"

value :: Parser Text
value = choice [ takeWhile1 (\c -> (not $ isSpace c) && c /= '"')
               , char '"' *> takeTill (=='"') <* char '"'
               ]
               <?> "value"

empty :: Parser ()
empty = skipSpace >> endOfInput

-- This makes sure we can parse all lines
optionLine :: Parser (Text, Text)
optionLine = do
      cKey <- identifier
      skipSpace
      char '='
      skipSpace
      cVal <- value
      skipSpace
      option () comment <?> "maybe comment after value"
      endOfInput
      return (cKey, cVal)
     <?> "option line"

configLine :: Parser (Maybe (Text,Text))
configLine = do
    e <- eitherP (choice [empty, comment >> endOfInput]) optionLine
    return $ case e
             of Left  _  -> Nothing
                Right kv -> Just kv

parseConfigLines :: Text -> Either String [(Text,Text)]
parseConfigLines = fmap catMaybes . els
                     where 
                        les = map (parseOnly $ configLine) . lines   :: Text -> [Either String (Maybe (Text, Text))]
                        els = sequence . les                         :: Text ->  Either String [Maybe (Text, Text)] 

parseConfig :: Text -> Either String Config
parseConfig = fmap (Config . Map.fromList) . parseConfigLines


loadConfig :: FilePath -> IO (Either String Config)
loadConfig = fmap parseConfig . readFile

