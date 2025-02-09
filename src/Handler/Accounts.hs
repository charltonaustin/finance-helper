{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Accounts where
import           Data.Text                (Text, pack)
import Foundation ( DB, Handler )
import Import.NoFoundation
    ( ($),
      Semigroup((<>)),
      widgetFile,
      Comment,
      Entity,
      (.),
      toHtml,
      setTitle,
      selectList,
      Html,
      Entity(entityVal),
      SelectOpt(Asc),
      EntityField(CommentId),
      Comment(commentMessage),
      Yesod(defaultLayout),
      YesodPersist(runDB) )

getAccountsR :: Handler Html
getAccountsR = do
  allComments <- runDB $ getAllComments

  defaultLayout $ do
    let (_, _, commentListId) = commentIds
    setTitle . toHtml $ (pack "Charlton")  <> "'s User page"
    $(widgetFile "accounts")

deleteAccountsR :: Handler Html
deleteAccountsR = do
  allComments <- runDB $ getAllComments

  defaultLayout $ do
    let (_, _, commentListId) = commentIds
    setTitle . toHtml $ (pack "Charlton")  <> "'s User page"
    $(widgetFile "accounts")

getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
