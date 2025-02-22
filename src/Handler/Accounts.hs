{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Accounts where

import Data.List (intercalate, splitAt, unfoldr)
import Service.Accounting (divideAndRound,positiveNegativeFormat, formatPercent)
import qualified Data.Map as Map
import Data.Text (pack, unpack)
import Text.Printf (printf)
import Foundation (DB, Handler, Route (AssetAccountR, NewAssetAccountR))
import Import.NoFoundation
  ( AssetAccount (..),
    AssetDeposit (..),
    AssetWithdrawal(..),
    AssetAccountId,
    SqlBackend,
    PersistEntityBackend,
    PersistEntity,
    Entity (entityVal),
    EntityField (AssetAccountId),
    Html,
    Int,
    Maybe (..),
    SelectOpt (Asc),
    Semigroup ((<>)),
    String,
    Yesod (defaultLayout),
    YesodPersist (runDB),
    Show,
    Eq,
    abs,
    div,
    entityKey,
    fst,
    map,
    mod,
    reverse,
    selectList,
    setTitle,
    show,
    snd,
    sum,
    toHtml,
    widgetFile,
    (+),
    (*),
    (<),
    (>),
    (-),
    ($),
    (++),
    (.),
    (>=), PassiveAssetChange,
  )
import Text.Printf (printf)

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

getAccountsR :: Handler Html
getAccountsR = do
  allAccounts <- runDB getAllAccounts
  deposits <- runDB getAllDeposits
  withdrawals <- runDB getAllWithdrawals
  passiveChanges <- runDB getAllPassiveChanges
  let accountDepositSum = sumAccountValue allAccounts deposits assetDepositAssetAccountId assetDepositValue
  let withdrawalSum = sumAccountValue allAccounts withdrawals assetWithdrawalAssetAccountId assetWithdrawalValue
  let passiveSum = sumAccountValue allAccounts withdrawals assetWithdrawalAssetAccountId passiveChanges
  let transactions = toTransactionsWith allAccounts accountDepositSum withdrawalSum passiveSum
  defaultLayout $ do
    setTitle . toHtml $ pack "Charlton" <> "'s User page"
    $(widgetFile "accounts")

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
toTransactionsWith allAccounts deposit withdrawal passiveChange =
  let depositMap = Map.fromList [(entityKey acc, d) | (acc, d) <- deposit]
      withdrawalMap = Map.fromList [(entityKey acc, w) | (acc, w) <- withdrawal]
      passiveMap = Map.fromList [(entityKey acc, w) | (acc, w) <- passiveChange]
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

-- | Format an Int as a dollar amount (e.g., 50000 -> "$500.00", 50 -> "$0.50", 5 -> "$0.05")
formatDollars :: Int -> String
formatDollars amount =
  let dollars = show ((abs amount) `div` 100)
      cents = amount `mod` 100
      commaDollars = if (abs amount) >= 100 then addCommas dollars else dollars
      formattedDollars = printf "$%s.%02d" commaDollars cents
   in positiveNegativeFormat amount formattedDollars

-- | Add commas to a numeric string (for thousands separator)
addCommas :: String -> String
addCommas num = reverse . intercalate "," . unfoldr splitThousands $ reverse num
  where
    splitThousands [] = Nothing
    splitThousands xs = Just (splitAt 3 xs)

getColor :: Int -> String
getColor i =
  if i >= 0
     then "text-success"
     else "text-danger"
