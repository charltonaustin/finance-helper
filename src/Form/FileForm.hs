{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Form.FileForm where
import Foundation
  (
    Form
  )
import Import.NoFoundation
  (
    FieldSettings
      (
        FieldSettings
      , fsAttrs
      , fsId
      , fsLabel
      , fsName
      , fsTooltip
      )
  , Text
  , FileInfo
  , Maybe(..)
  , textField
  , areq
  , fileAFormReq
  , selectFieldList
  , (<*>)
  , (<$>)
  , ($)
  )

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- Define our data that will be used for creating the form.
data FileForm = FileForm
  {
    fileInfo :: FileInfo
  , fileDescription :: Text
  , selectedOption :: Text
  }

fileForm :: [(Text, Text)] -> Form FileForm
fileForm options =
  renderBootstrap3 BootstrapBasicForm $
    FileForm
      <$> fileAFormReq "Choose a file"
      <*> areq textField textSettings Nothing
      <*> areq (selectFieldList options) "Choose an option" Nothing
  where
    -- Add attributes like the placeholder and CSS classes.
    textSettings =
      FieldSettings
        { fsLabel = "What's on the file?",
          fsTooltip = Nothing,
          fsId = Nothing,
          fsName = Nothing,
          fsAttrs =
            [ ("class", "form-control"),
              ("placeholder", "File description")
            ]
        }

