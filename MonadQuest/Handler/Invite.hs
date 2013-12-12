{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Invite where

import Import
import Facebook
import Yesod.Auth.Facebook

getInviteR :: Handler Html
getInviteR = do
  maid <- maybeAuthId
  case maid of
    Nothing -> redirect ("/" :: Text)
    Just aid -> do
      mtoken <- getUserAccessToken
      case mtoken of
        Nothing -> redirect ("/" :: Text)
        Just token -> do
          friends <- runYesodFbT $ do
            Pager frnds _ _ <- getUserFriends "me" [] token
            return $ map friendName frnds
          defaultLayout $ do
            $(widgetFile "invite")

postInviteR :: Handler Html
postInviteR = getInviteR
