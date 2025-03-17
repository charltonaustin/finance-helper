{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.AssetAccount where

import Foundation (Handler, Route (AccountsR))
import Import.NoFoundation (AssetAccountId, Html, delete, redirect, runDB, setMessage, ($))

deleteAssetAccountR :: AssetAccountId -> Handler Html
deleteAssetAccountR accountId = do
  runDB $ delete accountId
  setMessage "Account deleted successfully"
  redirect AccountsR

getAssetAccountR :: AssetAccountId -> Handler Html
getAssetAccountR accountId = do
  redirect AccountsR

putAssetAccountR :: AssetAccountId -> Handler Html
putAssetAccountR accountId = do
  setMessage "Do a put"
  redirect AccountsR
