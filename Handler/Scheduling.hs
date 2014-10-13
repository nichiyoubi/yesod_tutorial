module Handler.Scheduling where

import Import
import System.Time

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
	cal <- liftIO $ getClockTime
	let year = ctYear (toUTCTime cal)
	let mon = (fromEnum (ctMonth (toUTCTime cal))) + 1
	defaultLayout $(widgetFile "scheduling")