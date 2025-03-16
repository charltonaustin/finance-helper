{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.Accounting where

import Text.Printf (printf)

import Prelude hiding ()
-- Function to divide two Ints and return a Double rounded to two decimal places
divideAndRound :: Int -> Int -> Double
divideAndRound _ 0 = 0
divideAndRound x y = 100 * fromIntegral x / fromIntegral y

positiveNegativeFormat :: (Num a, Ord a) => a -> String -> String
positiveNegativeFormat amount currentString =
  if amount < 0 then printf "(-%s)" currentString else printf "+%s" currentString


formatPercent :: Double -> String
formatPercent amount =
  positiveNegativeFormat amount (printf "%.2f%%" (abs amount))
