{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  result <- runDB $ selectList [] [LimitTo 2, Desc ArticleDate] :: Handler [Entity Article]
  mAuthId <- maybeAuthId
  defaultLayout $(widgetFile "home")
