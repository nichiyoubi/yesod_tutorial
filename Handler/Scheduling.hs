module Handler.Scheduling where

import Import

data Scheduling = Scheduling
        { title :: Text,
	  comment :: Maybe Textarea
	}
	deriving Show

schedulingForm :: Html -> MForm Handler (FormResult Scheduling, Widget)
schedulingForm = renderDivs $ Scheduling
	<$> areq textField "タイトル" Nothing
	<*> aopt textareaField "コメント" Nothing

getSchedulingR :: Handler Html
getSchedulingR = do
	(widget, enctype) <- generateFormPost schedulingForm
	defaultLayout $(widgetFile "scheduling")