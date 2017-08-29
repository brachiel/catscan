module Config
    ( scanCatBasePath
    , configFile
    , keywordsFile
    , docExt
    , docBasePath
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
scanCatBasePath = "../" :: FilePath
configFile = scanCatBasePath ++ "config.sh" :: FilePath
keywordsFile = (++) scanCatBasePath <$> config # "KEYWORD_FILE" :: IO FilePath
docBasePath = (++) scanCatBasePath <$> config # "DOCUMENT_DIR" :: IO FilePath
docExt = fmap tail $ config # "SCAN_EXT" :: IO String

type Config = Map.Map Text Text

config :: IO Config
config = do
    ec <- loadConfig configFile
    return $ case ec
             of Left  err    -> error err
                Right config -> config


-- |Read key from (IO Config)
(#) :: IO Config -> String -> IO String
config # key = do
    cfg <- config
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
parseConfig = fmap Map.fromList . parseConfigLines


loadConfig :: FilePath -> IO (Either String Config)
loadConfig = fmap parseConfig . readFile

