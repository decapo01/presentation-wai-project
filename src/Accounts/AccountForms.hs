module Accounts.AccountForms where


import ClassyPrelude

-- import Control.Applicative ((<$>),(<*>))
import Text.Digestive
import Text.Digestive.Util
import qualified Text.Digestive.Form  as DF
import qualified Text.Digestive.Types as DF 
import Text.Regex

data LoginDto
  = LoginDto
  { loginDtoEmail :: Text
  , loginDtoPass  :: Text
  }
  deriving (Show)


loginForm :: Monad m => Form [Text] m LoginDto
loginForm = LoginDto
  <$> "email" .: DF.validate emailValidation (DF.text Nothing)
  <*> "pass"  .: DF.validate passwordValidation (DF.text Nothing)

minMaxLen :: MonoFoldable a => Int -> Int -> a -> DF.Result Text a
minMaxLen min max item =
  if length item >= min && length item <= max
    then DF.Success item
    else DF.Error $ "Item should be between " <> tshow min <> " and " <> tshow max <> " length"


required :: Text -> DF.Result [Text] Text
required item =
  if length item /= 0
    then DF.Success item
    else DF.Error   ["required"]

matchesRegex :: v -> String -> Text -> DF.Result v Text
matchesRegex errMsg regexStr str =
  if isJust . matchRegex (mkRegexWithOpts regexStr True True) . unpack $ str
    then DF.Success str
    else DF.Error errMsg

emailValidation :: Text -> Result [Text] Text
emailValidation = conditions [matchesRegex "Not a valid email" "^[a-zA-Z0-9\\.\\+\\-]+@[a-zA-Z0-9]+\\.[a-zA-Z0-9]+$"]

passwordValidation :: Text -> DF.Result [Text] Text
passwordValidation = DF.conditions [minMaxLen 8 18]