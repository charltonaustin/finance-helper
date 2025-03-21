module Handler.Comment where

import Foundation ( Handler )
import Import.NoFoundation
    ( Comment(commentUserId),
      YesodPersist(runDB),
      Value,
      insertEntity,
      requireCheckJsonBody,
      returnJson,
      YesodAuth(maybeAuthId) )

postCommentR :: Handler Value
postCommentR = do
  -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
  -- (The ToJSON and FromJSON instances are derived in the config/models file).
  comment <- (requireCheckJsonBody :: Handler Comment)

  -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
  maybeCurrentUserId <- maybeAuthId
  let comment' = comment {commentUserId = maybeCurrentUserId}

  insertedComment <- runDB $ insertEntity comment'
  returnJson insertedComment
