{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Accounts where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Data.Time (Day, getCurrentTime, utctDay)
import Foundation (DB, Form, Handler, Route (AccountsR))
import Import.NoFoundation
  ( Applicative ((<*>)),
    Asset (..),
    AssetAccount (..),
    AssetDeposit (..),
    AssetValue (..),
    Comment (commentMessage),
    Entity (entityVal),
    EntityField (AssetAccountId),
    FieldSettings
      ( FieldSettings,
        fsAttrs,
        fsId,
        fsLabel,
        fsName,
        fsTooltip
      ),
    Html,
    IO,
    Maybe (..),
    SelectOpt (Asc),
    Semigroup ((<>)),
    Yesod (defaultLayout),
    YesodPersist (runDB),
    aopt,
    entityKey,
    fmap,
    generateFormPost,
    insertEntity,
    selectList,
    setTitle,
    textField,
    toHtml,
    widgetFile,
    ($),
    (.),
    (<$>),
  )
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getAccountsR :: Handler Html
getAccountsR = do
  (_, formEnctype) <- generateFormPost sampleForm
  allAccounts <- runDB $ getAllAccounts

  defaultLayout $ do
    let (_, _, commentListId) = commentIds
    setTitle . toHtml $ (pack "Charlton") <> "'s User page"
    $(widgetFile "accounts")

deleteAccountsR :: Handler Html
deleteAccountsR = do
  (_, formEnctype) <- generateFormPost sampleForm
  allAccounts <- runDB $ getAllAccounts

  defaultLayout $ do
    let (_, _, commentListId) = commentIds
    setTitle . toHtml $ (pack "Charlton") <> "'s User page"
    $(widgetFile "accounts")

postAccountsR :: Handler Html
postAccountsR = do
  (_, formEnctype) <- generateFormPost sampleForm
  today <- liftIO getToday
  accountEntity <- runDB $ insertEntity $ AssetAccount "Fidelity Individual" "...555"
  let accountId = entityKey accountEntity -- Extract primary key
  _ <- runDB $ insertEntity $ AssetDeposit 50000 accountId today
  allAccounts <- runDB $ getAllAccounts

  defaultLayout $ do
    let (_, _, commentListId) = commentIds
    setTitle . toHtml $ (pack "Charlton") <> "'s User page"
    $(widgetFile "accounts")

getAllAccounts :: DB [Entity AssetAccount]
getAllAccounts = selectList [] [Asc AssetAccountId]

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

getToday :: IO Day
getToday = fmap utctDay getCurrentTime

data AccountForm = AccountForm
  { name :: Maybe Text
  }

sampleForm :: Form AccountForm
sampleForm =
  renderBootstrap3 BootstrapBasicForm $ AccountForm <$> aopt textField textSettings Nothing
  where
    -- Add attributes like the placeholder and CSS classes.
    textSettings =
      FieldSettings
        { fsLabel = "Name",
          fsTooltip = Nothing,
          fsId = Nothing,
          fsName = Nothing,
          fsAttrs =
            [ ("class", "form-control"),
              ("placeholder", "Name")
            ]
        }
