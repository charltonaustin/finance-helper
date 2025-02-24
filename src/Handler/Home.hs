{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where
import Form.FileForm
import Foundation
  (
    DB
  , Handler
  , Route (AuthR, CommentR, HomeR, ProfileR)
  )
import Import.NoFoundation
  (
    Comment (commentMessage)
  , Entity (entityVal)
  , EntityField (CommentId)
  , FileInfo (fileContentType)
  , FormResult (FormSuccess)
  , Html
  , Maybe (..)
  , SelectOpt (Asc)
  , Text
  , ToJSON (toJSON)
  , Yesod (defaultLayout)
  , YesodPersist (runDB)
  , Route(LoginR)
  , generateFormPost
  , newIdent
  , runFormPost
  , selectList
  , setTitle
  , widgetFile
  , ($)
  )
import Text.Julius (RawJS (..))

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  (formWidget, formEnctype) <- generateFormPost (fileForm [])
  let submission = Nothing :: Maybe FileForm
      handlerName = "getHomeR" :: Text
  allComments <- runDB $ getAllComments

  defaultLayout $ do
    let (commentFormId, commentTextareaId, commentListId) = commentIds
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
  ((result, formWidget), formEnctype) <- runFormPost (fileForm [])
  let handlerName = "postHomeR" :: Text
      submission = case result of
        FormSuccess res -> Just res
        _ -> Nothing
  allComments <- runDB $ getAllComments

  defaultLayout $ do
    let (commentFormId, commentTextareaId, commentListId) = commentIds
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]
