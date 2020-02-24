{-# LANGUAGE FlexibleInstances #-}
module Accounts.AccountViews where

import ClassyPrelude
import Lucid
import Common.LucidBootstrapHelpers

data LoginErrors
  = LoginErrors
  { globalErrors :: [Text]
  , emailErrors  :: [Text]
  , passErrors   :: [Text]
  }


getOrElse :: Maybe a -> a -> a
getOrElse item alt =
  case item of
    Just i  -> i
    Nothing -> alt

errorOrEmpty :: Maybe LoginErrors -> [Text]
errorOrEmpty item =
  case item of
    Just _ -> [""]
    Nothing -> ["error"]
 
getEmailErrors :: Maybe LoginErrors -> [Text]
getEmailErrors item =
  case item of
    Nothing -> []
    Just i  ->
      if (length $ emailErrors i) == 0
        then []
        else emailErrors i

getPasswordErrors :: Maybe LoginErrors -> [Text]
getPasswordErrors item =
  case item of
    Nothing -> []
    Just i  ->
      if (length $ passErrors i) == 0
        then []
        else emailErrors i

data LoginForm
  = LoginForm
  { loginFormEmail :: Text
  , loginFormPass  :: Text
  , loginFormGlobalErrors :: [Text]
  , loginFormEmailErrors  :: [Text]
  , loginFormPassErrors   :: [Text]
  }

defaultLoginForm = LoginForm "" "" [] [] []

loginView :: LoginForm -> Html ()
loginView form = do
  container_fluid_ $ do
    row_ $ do
      col_12_ $ do
        if (length globalErrs) /= 0
          then  do
            div_ [classes_ ["alert alert-danger"]] $ do
              forM_  globalErrs $ \(er) -> do
                li_ $ toHtml er
              -- ul_ $ sequence_ [li_ $ toHtml x | x <- globalErrs]
          else
            emptyHtml_
    row_ $ do
      col_12_ $ do
        form__ $ do
          form_group_text_ "Email" "loginForm.email" (loginFormEmail form) (loginFormEmailErrors form)
          form_group_password_ "Password" "loginForm.pass" (loginFormPass form) (loginFormPassErrors form)
          input_submit_ "Login"
  where
    x = map (\e -> "dkljsflds") loginFormGlobalErrors form
    globalErrs = loginFormGlobalErrors form

emptyHtml_ = ""