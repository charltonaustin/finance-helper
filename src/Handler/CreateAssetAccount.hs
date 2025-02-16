{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.CreateAssetAccount where

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

postCreateAssetAccountR  :: Handler Html
postCreateAssetAccountR = do
  ((result, _), _) <- runFormPost assetAccountForm
  case result of
    FormSuccess formData -> do
      insertNewAccount formData
      setMessage "Created a new account."
    _ -> setMessage "Failed to create a new account try again."
  redirect AccountsR

insertNewAccount :: AccountForm -> Handler ()
insertNewAccount account = do
  randomDeposit <- liftIO (randomInRange 1 100000000000)
  randomWithdrawal <- liftIO (randomInRange 1 100000000000)
  today <- liftIO getToday
  accountEntity <- runDB $ insertEntity $ AssetAccount (name account) (accountNumber account) (accountType account)
  let accountId = entityKey accountEntity
  _ <- runDB $ insertEntity $ AssetDeposit (abs randomDeposit) accountId today
  _ <- runDB $ insertEntity $ AssetWithdrawal (abs randomWithdrawal) accountId today
  return ()

getToday :: IO Day
getToday = fmap utctDay getCurrentTime

randomInRange :: Int -> Int -> IO Int
randomInRange low high = do
  r <- randomIO :: IO Int
  return (low + (r `mod` (high - low + 1)))
