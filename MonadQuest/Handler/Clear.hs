{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Clear where

import Import

getClearR :: Handler Html
getClearR = do
  maid <- maybeAuthId
  case maid of
    Nothing -> redirect ("/" :: Text)
    Just aid -> do
      muser <- runDB $ get aid
      case muser of
        Nothing -> redirect ("/" :: Text)
        Just user -> do
          mstate <- runDB $ getBy $ UniqueState user
          case mstate of
            Nothing -> redirect ("/" :: Text)
            Just (Entity stId (UserState _ ul _ _)) -> do
              runDB $ update stId [ UserStateProgress =. 0, UserStateLevel =. ul + 1]
      defaultLayout $ do
        $(widgetFile "clear")

postClearR :: Handler Html
postClearR = getClearR
