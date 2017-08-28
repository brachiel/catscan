module Config
    ( Config
    , loadConfig
    , parseConfig
    , parseConfigLines
    , configLine
    ) where

-- Based on http://vaibhavsagar.com/blog/2017/08/13/i-haskell-a-git/
-- and http://www.serpentine.com/blog/2007/01/31/parsing-a-simple-config-file-in-haskell/

import Prelude hiding (takeWhile, lines, readFile)
import Data.Attoparsec.Text
import Data.Char (isAlpha, isSpace)
import Data.Text (Text, pack, lines)
import Data.Text.IO (readFile)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import Control.Applicative ((<|>))
import Control.Monad (sequence)

-- TODO: Parse comments and empty lines, too

type Config = Map.Map Text Text

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

