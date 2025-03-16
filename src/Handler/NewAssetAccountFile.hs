{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.NewAssetAccountFile where


import Form.FileForm
import Data.Text (pack)
import Foundation (Handler, Route (CreateAssetAccountsFileR))
import Import.NoFoundation
  (
    Html
  , AssetAccount(..)
  , generateFormPost
  , widgetFile
  , toHtml
  , setTitle
  , defaultLayout
  , map
  , runDB
  , (<>)
  , ($)
  , (.)
  , Entity (entityVal)
  )
import Service.AssetAccount (getAllAccounts)

getNewAssetAccountFileR :: Handler Html
getNewAssetAccountFileR = do
  accounts <- runDB $ getAllAccounts
  let availableAccounts = map (\a -> ((assetAccountName (entityVal a)), (assetAccountAccountNumber (entityVal a)))) accounts
  (formWidget, formEnctype) <- generateFormPost (fileForm availableAccounts)
  defaultLayout $ do
    setTitle . toHtml $ pack "Charlton" <> "'s User page"
    $(widgetFile "new-asset-account-file")
