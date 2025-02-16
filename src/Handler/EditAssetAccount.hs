{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.EditAssetAccount where

import Foundation (Handler, Route (AccountsR))

import Import.NoFoundation (AssetAccountId, Html, delete, redirect, runDB, setMessage, ($))

getEditAssetAccountR :: AssetAccountId -> Handler Html
getEditAssetAccountR accountId = do
  redirect AccountsR

