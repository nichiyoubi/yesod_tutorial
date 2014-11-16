module Handler.Register where

import Import

getRegisterR :: CandidateListId -> Handler Html
getRegisterR listId = do
        days <- runDB $ selectList [CandidateDaysListId ==. listId] []
	defaultLayout $(widgetFile "register")

postRegisterR :: CandidateListId -> Handler Html
postRegisterR = error "Not yet implemented: postRegisterR"
