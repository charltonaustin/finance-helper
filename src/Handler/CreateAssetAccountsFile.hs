{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.CreateAssetAccountsFile where

import Form.FileForm

import Import.NoFoundation

import Foundation

postCreateAssetAccountsFileR  :: Handler Html
postCreateAssetAccountsFileR = do
  ((result, _), _) <- runFormPost (fileForm [])
  case result of
    FormSuccess file -> do
      let filename = unpack $ fileName (fileInfo file)  -- Get file name
      let filepath = "static/uploads/" ++ filename -- Define save path
      liftIO $ fileMove (fileInfo file) filepath -- Save the file
      setMessage "Created a new file"
    _ -> setMessage "Failed to create a file try again."
  redirect AccountsR

