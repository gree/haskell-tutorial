module Handler.GetMore where

import Import

getGetMoreR :: Handler Value
getGetMoreR = do
  from' <- fmap (fromMaybe "0") $ lookupGetParam "from"
  let from = read (unpack from') :: Int
  result' <- runDB $ selectList [] [OffsetBy from, LimitTo 2, Desc ArticleDate]
               :: Handler [Entity Article]
  let result = map entityVal result'
  returnJson result
