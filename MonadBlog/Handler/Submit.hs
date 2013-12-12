module Handler.Submit where

import Import

import Control.Exception.Lifted (try, SomeException)

data ArticleForm = ArticleForm Text Textarea

articleForm :: Html -> MForm Handler (FormResult ArticleForm, Widget)
articleForm = renderDivs $ ArticleForm
  <$> areq textField "Title" Nothing
  <*> areq textareaField "Text" Nothing

getSubmitR :: Handler Html
getSubmitR = do
  auth <- requireAuth
  ((_, formWidget), enctype) <- runFormPost articleForm
  defaultLayout $(widgetFile "submit")

postSubmitR :: Handler Html
postSubmitR = do
  auth <- requireAuth
  ((result, formWidget), enctype) <- runFormPost articleForm
  case result of
    FormSuccess (ArticleForm title textarea) -> do
      currentTime <- liftIO $ getCurrentTime
      result' <- runDB $ try $ insert (Article title currentTime (unTextarea textarea))
                   :: Handler (Either SomeException (Key Article))
      case result' of
        Left _ -> redirect HomeR  -- TODO: error handling
        Right _ -> redirect HomeR
      
    _ -> defaultLayout $(widgetFile "submit")
