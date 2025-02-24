{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.AssetAccount where
import Foundation (DB)
import Import.NoFoundation
  (
    AssetAccount (..)
  , AssetDeposit (..)
  , PassiveAssetChange (..)
  , AssetWithdrawal (..)
  , Entity
  , SqlBackend
  , ReaderT
  , MonadIO
  , YesodPersist
  , EntityField (AssetAccountId)
  , AssetAccountId
  , PersistEntityBackend
  , PersistEntity
  , SelectOpt (Asc)
  , YesodPersistBackend
  , Int
  , Eq
  , String
  , Show
  , entityKey
  , entityVal
  , selectList
  , return
  , ($)
  , (+)
  , (-)
  , runDB
  , HandlerFor
  )

import qualified Data.Map as Map
import Data.Text (unpack)

data AccountTransaction = AccountTransaction
    { name                      :: String
    , number                    :: String
    , accountType               :: String
    , passiveChange             :: Int
    , depositWithdrawalGainLoss :: Int
    , deposit                   :: Int
    , withdrawal                :: Int
    , currentValue              :: Int
    , assetAccountId            :: AssetAccountId
    } deriving (Show, Eq)


getAllData :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend)
           => HandlerFor site ([Entity AssetAccount], [Entity AssetDeposit], [Entity AssetWithdrawal], [Entity PassiveAssetChange])
getAllData = runDB $ do
  allAccounts <- getAllAccounts
  deposits <- getAllDeposits
  withdrawals <- getAllWithdrawals
  passiveChanges <- getAllPassiveChanges
  return (allAccounts, deposits, withdrawals, passiveChanges)

sumAllData :: [Entity AssetAccount] -> [Entity AssetDeposit] -> [Entity AssetWithdrawal] -> [Entity PassiveAssetChange] -> [AccountTransaction]
sumAllData allAccounts deposits withdrawals passiveChanges =
  let accountDepositSum = sumAccountValue allAccounts deposits assetDepositAssetAccountId assetDepositValue
      withdrawalSum = sumAccountValue allAccounts withdrawals assetWithdrawalAssetAccountId assetWithdrawalValue
      passiveSum = sumAccountValue allAccounts passiveChanges passiveAssetChangeAssetAccountId passiveAssetChangeValue
      assetTransactions = toTransactionsWith allAccounts accountDepositSum withdrawalSum passiveSum
  in assetTransactions


sumAccountValue :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
                => [Entity AssetAccount]
                -> [Entity record]
                -> (record -> AssetAccountId)
                -> (record -> Int)
                -> [(Entity AssetAccount, Int)]
sumAccountValue allAccounts values getAccountId getValue =
  let valueMap = Map.fromListWith (+) [(getAccountId (entityVal v), getValue (entityVal v)) | v <- values]
      accountValues = [(acc, Map.findWithDefault 0 (entityKey acc) valueMap) | acc <- allAccounts]
  in accountValues

toTransactionsWith :: [Entity AssetAccount] -> [(Entity AssetAccount, Int)] -> [(Entity AssetAccount, Int)] -> [(Entity AssetAccount, Int)] -> [AccountTransaction]
toTransactionsWith allAccounts deposits withdrawals passiveChanges =
  let depositMap = Map.fromList [(entityKey acc, d) | (acc, d) <- deposits]
      withdrawalMap = Map.fromList [(entityKey acc, w) | (acc, w) <- withdrawals]
      passiveMap = Map.fromList [(entityKey acc, w) | (acc, w) <- passiveChanges]
  in
    [ AccountTransaction
        { name                      = unpack $ assetAccountName (entityVal acc)
        , number                    = unpack $ assetAccountAccountNumber (entityVal acc)
        , accountType               = unpack $ assetAccountType (entityVal acc)
        , passiveChange             = p
        , depositWithdrawalGainLoss = d - w
        , deposit                   = d
        , withdrawal                = w
        , currentValue              = d - w + p
        , assetAccountId            = entityKey acc
        }
    | acc <- allAccounts
    , let d = Map.findWithDefault 0 (entityKey acc) depositMap
    , let w = Map.findWithDefault 0 (entityKey acc) withdrawalMap
    , let p = Map.findWithDefault 0 (entityKey acc) passiveMap
    ]

getAllPassiveChanges :: DB [Entity PassiveAssetChange]
getAllPassiveChanges = selectList [] []

getAllAccounts :: DB [Entity AssetAccount]
getAllAccounts = selectList [] [Asc AssetAccountId]

getAllDeposits :: DB [Entity AssetDeposit]
getAllDeposits = selectList [] []

getAllWithdrawals :: DB [Entity AssetWithdrawal]
getAllWithdrawals = selectList [] []

