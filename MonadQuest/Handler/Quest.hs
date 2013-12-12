{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Quest where

import Import

getQuestR :: Int -> Handler Html
getQuestR questLv = do
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
            Just (Entity stId (UserState uu ul uprogress ustamina)) -> do
              mquest <- runDB $ getBy $ UniqueQuest questLv
              case mquest of
                Nothing -> notFound
                Just (Entity quId (Quest ql qn qlen)) -> do
                  if ustamina == 0
                    then
                      redirect ("/" :: Text)
                    else if uprogress <= qlen
                    then do
                      let isBoss = uprogress == qlen
                      runDB $ update stId [ UserStateProgress =. uprogress + 1
                                          , UserStateStamina =. ustamina - 1]
                      defaultLayout $ do
                        $(widgetFile "quest")
                    else do
                      redirect ("/clear" :: Text)

postQuestR :: Int -> Handler Html
postQuestR = getQuestR
