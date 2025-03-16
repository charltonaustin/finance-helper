{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.CreateAssetAccounts where

import Foundation (Handler, App, Route (AccountsR))

import System.Random

import Form.AssetAccountForm (AccountForm(..), assetAccountForm)
import Control.Monad.IO.Class (liftIO)
import Data.Time (Day, getCurrentTime, utctDay)
import Import.NoFoundation
  (
    Html,
    FormResult(..),
    IO,
    AssetAccount(..),
    PassiveAssetChange(..),
    AssetDeposit (..),
    AssetWithdrawal (..),
    Int,
    fmap,
    abs,
    redirect,
    setMessage,
    insertEntity,
    runDB,
    runFormPost,
    entityKey,
    return,
    mod,
    ($),
    (*),
    (+),
    (-),
  )

postCreateAssetAccountsR  :: Handler Html
postCreateAssetAccountsR = do
  ((result, _), _) <- runFormPost assetAccountForm
  case result of
    FormSuccess formData -> do
      insertNewAccount formData
      setMessage "Created a new account."
    _ -> setMessage "Failed to create a new account try again."
  redirect AccountsR

insertNewAccount :: AccountForm -> Handler ()
insertNewAccount account = do
  _ <- runDB $ insertEntity $ AssetAccount (name account) (accountNumber account) (accountType account)
  return ()

getToday :: IO Day
getToday = fmap utctDay getCurrentTime

randomInRange :: Int -> Int -> IO Int
randomInRange low high = do
  r <- randomIO :: IO Int
  return (low + (r `mod` (high - low + 1)))
