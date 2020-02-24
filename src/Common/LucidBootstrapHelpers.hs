module Common.LucidBootstrapHelpers where

import ClassyPrelude
import Lucid
import Data.Text

isEmpty :: MonoFoldable a => a -> Bool
isEmpty item =
  if ClassyPrelude.length item == 0
    then True
    else False

isNotEmpty :: MonoFoldable a => a -> Bool
isNotEmpty item =
  if ClassyPrelude.length item == 0
    then False
    else True

hasError :: [Text] -> Text
hasError errors =
  if isEmpty errors
    then ""
    else "has-error"

container_fluid_ :: Term arg result => arg -> result
container_fluid_ = termWith "div" [class_ " container-fluid "]

row_ :: Term arg result => arg -> result
row_ = termWith "div" [class_ " row "]

col_12_ :: Term arg result => arg -> result
col_12_  = termWith "div" [class_ "col-lg-12 col-md-12 col-sm-12 col-xs-12"]


form__ :: Html () -> Html ()
form__ content_ = do
  form_ [classes_ ["form"], method_ "post"] $ do
    content_

form_group_text_ :: Text -> Text -> Text -> [Text] -> Html ()
form_group_text_ label name value errors = do
  div_ [classes_ ["form-group", hasError errors]] $ do
    label_ [classes_ ["control-label"]] (toHtml label)
    input_ [classes_ ["form-control"], type_ "text", name_ name, value_ value]
    span_ [classes_ ["help-block"]] $ toHtml $ ClassyPrelude.intercalate ", " errors

form_group_password_ :: Text -> Text -> Text -> [Text] -> Html ()
form_group_password_ label name value errors = do
  div_ [classes_ ["form-group", hasError errors]] $ do
    label_ [classes_ ["control-label"]] (toHtml label)
    input_ [classes_ ["form-control"], type_ "password", name_ name, value_ value]
    span_ [classes_ ["help-block"]] $ toHtml $ ClassyPrelude.intercalate ", " errors

input_submit_ :: Text -> Html ()
input_submit_ value = do
  input_ [classes_ ["btn btn-default"], type_ "submit", value_ value]