{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Accounts where


import Data.List (intercalate, splitAt, unfoldr)
import Service.Accounting (divideAndRound,positiveNegativeFormat, formatPercent)
import Service.AssetAccount (getAllData, AccountTransaction(..), sumAllData)
import Data.Text (pack)
import Text.Printf (printf)
import Foundation (Handler, Route (AssetAccountR, NewAssetAccountR, NewAssetAccountFileR))
import Import.NoFoundation
  ( Html,
    Int,
    Maybe (..),
    Semigroup ((<>)),
    String,
    Yesod (defaultLayout),
    abs,
    div,
    mod,
    reverse,
    setTitle,
    show,
    toHtml,
    widgetFile,
    (*),
    ($),
    (.),
    (>=),
  )

getAccountsR :: Handler Html
getAccountsR = do
  (allAccounts, deposits, withdrawals, passiveChanges) <- getAllData
  let assetTransactions = sumAllData allAccounts deposits withdrawals passiveChanges
  defaultLayout $ do
    setTitle . toHtml $ pack "Charlton" <> "'s User page"
    $(widgetFile "accounts")

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
