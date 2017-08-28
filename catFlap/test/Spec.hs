import Test.QuickCheck
import Test.QuickCheck.Instances -- allows Arbitrary Text

import qualified Data.Attoparsec.Text as AT
import qualified Data.Char as C
import qualified Data.Text as T

import qualified Config

main :: IO ()
main = do
    quickCheck prop_ignore_comments
    quickCheck prop_identity

makeSetLine :: (T.Text, T.Text) -> T.Text
makeSetLine (k,v) = k `T.append` ('=' `T.cons` v)

-- We drop the error message since we dont care WHY it fails
parseConfigLine :: T.Text -> Either String (Maybe (T.Text, T.Text))
parseConfigLine t = case AT.parseOnly Config.configLine t
                    of Left  _ -> Left "error"
                       Right r -> Right r

prop_ignore_comments :: T.Text -> Property
prop_ignore_comments t = not (T.null t) ==>
    parseConfigLine t == parseConfigLine (t `T.append` T.pack "# Comment")

prop_identity :: (T.Text, T.Text) -> Property
prop_identity (k,v) = not (T.null k) && T.all C.isAlpha k && not (T.null v) && not (T.all C.isSpace v)  ==>
    (parseConfigLine . makeSetLine) (k,v) == (Right $ Just (k, v))

