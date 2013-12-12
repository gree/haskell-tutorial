{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Select where

import Import

getSelectR :: Handler Html
getSelectR = do
  maid <- maybeAuthId
  case maid of
    Nothing -> redirect ("/" :: Text)
    Just aid -> do
      muser <- runDB $ get aid
      case muser of
        Nothing -> redirect ("/" :: Text)
        Just user -> do
          mstate <- runDB $ getBy $ UniqueState user
          UserState _ level _ _ <- case mstate of
            Just (Entity lvId lv) -> do
              runDB $ update lvId [UserStateProgress =. 0]
              return lv
            Nothing -> do
              let newLv = UserState user 1 0 1000
              _ <- runDB $ insert newLv
              return newLv
          mquests <- runDB $ selectList [QuestLevel <=. level] []
          let quests = map (\(Entity _ q) -> q) mquests
          defaultLayout $ do
            $(widgetFile "select")

postSelectR :: Handler Html
postSelectR = getSelectR
