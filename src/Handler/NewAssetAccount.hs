{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.NewAssetAccount where

import Foundation (Handler, Route (CreateAssetAccountsR))

import Form.AssetAccountForm (assetAccountForm)

import Data.Text (pack)

import Import.NoFoundation(
   Html,
   widgetFile,
   generateFormPost,
   defaultLayout,
   setTitle,
   toHtml,
   (<>),
   (.),
   ($)
  )

getNewAssetAccountR :: Handler Html
getNewAssetAccountR = do
  (formWidget, formEnctype) <- generateFormPost assetAccountForm
  defaultLayout $ do
    setTitle . toHtml $ pack "Charlton" <> "'s User page"
    $(widgetFile "new-asset-account")

