{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.EditAssetAccount where

import Foundation (Handler, Route (AccountsR, AssetAccountR))
import Data.Text (pack)

import Form.AssetAccountForm (assetAccountForm)
import Import.NoFoundation (AssetAccountId, Html, ($), widgetFile, setTitle, (.), toHtml, Yesod(defaultLayout), Semigroup((<>)), generateFormPost)

getEditAssetAccountR :: AssetAccountId -> Handler Html
getEditAssetAccountR accountId = do
  (formWidget, formEnctype) <- generateFormPost assetAccountForm
  defaultLayout $ do
    setTitle . toHtml $ pack "Charlton" <> "'s User page"
    $(widgetFile "edit-asset-account")


