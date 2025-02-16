{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Form.AssetAccountForm where

import Foundation (Form)

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Data.Text (Text)

import Import.NoFoundation(
   FieldSettings
      ( FieldSettings,
        fsAttrs,
        fsId,
        fsLabel,
        fsName,
        fsTooltip
      ),
   Maybe(..),
   textField,
   areq,
   (<$>),
   (<*>),
   ($)
  )

data AccountForm = AccountForm
  {
    name :: Text,
    accountNumber :: Text,
    accountType :: Text
  }


assetAccountForm :: Form AccountForm
assetAccountForm =
  renderBootstrap3 BootstrapBasicForm $
    AccountForm
      <$> areq textField nameSettings Nothing
      <*> areq textField accountIdSettings Nothing
      <*> areq textField typeSetting Nothing
  where
    -- Add attributes like the placeholder and CSS classes.
    accountIdSettings =
      FieldSettings
        { fsLabel = "Account Id",
          fsTooltip = Nothing,
          fsId = Nothing,
          fsName = Nothing,
          fsAttrs =
            [ ("class", "form-control"),
              ("placeholder", "1234..")
            ]
        }
    typeSetting =
      FieldSettings
        { fsLabel = "Type",
          fsTooltip = Nothing,
          fsId = Nothing,
          fsName = Nothing,
          fsAttrs =
            [ ("class", "form-control"),
              ("placeholder", "1234..")
            ]
        }
    nameSettings =
      FieldSettings
        { fsLabel = "Name",
          fsTooltip = Nothing,
          fsId = Nothing,
          fsName = Nothing,
          fsAttrs =
            [ ("class", "form-control"),
              ("placeholder", "Fidelity")
            ]
        }
