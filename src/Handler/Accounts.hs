{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Accounts where
import           Data.Text                (Text, pack)
import Import
    ( ($),
      Semigroup((<>)),
      widgetFile,
      User(userIdent),
      (.),
      toHtml,
      setTitle,
      Html,
      Yesod(defaultLayout),
      Handler )

getAccountsR :: Handler Html
getAccountsR = do
    defaultLayout $ do
        setTitle . toHtml $ (pack "Charlton")  <> "'s User page"
        $(widgetFile "accounts")
