module Handler.Register where

import Import

getRegisterR :: CandidateListId -> Handler Html
-- getRegisterR = error "Not yet implemented: getRegisterR"
getRegisterR listId = do
        days <- runDB $ selectList [CandidateDaysListId ==. listId] []
	defaultLayout [whamlet|
<h1>test
|]

postRegisterR :: CandidateListId -> Handler Html
postRegisterR = error "Not yet implemented: postRegisterR"
