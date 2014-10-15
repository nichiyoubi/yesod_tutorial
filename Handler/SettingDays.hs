module Handler.SettingDays where

import Import
import Data.Text
import Data.Time.Calendar
import Handler.Setting

data Days = Days
        { cdays :: [Day]
	}
	deriving Show

daysForm :: [(Text, Day)] -> Html -> MForm Handler (FormResult Days, Widget)
daysForm days = renderDivs $ Days
        <$> areq (checkboxesFieldList days) "***" Nothing

postSettingDaysR :: Handler Html
postSettingDaysR = do
        ((result, widget), enctype) <- runFormPost settingForm
	case result of
	        FormSuccess schedule -> do
		        (settingWidget, enctype') <- generateFormPost (daysForm days)
			defaultLayout $(widgetFile "settingDays")
			where end   = endDay schedule
			      start = startDay schedule
			      diff  = diffDays end start
			      days  = getCandidateDays end start
		_ -> defaultLayout
		        [whamlet|
<p>Invalid input, let's try again.
|]

getCandidateDays :: Day -> Day -> [(Text, Day)] 
getCandidateDays end start = getCandidateDays' diff diff start
		     	     where diff = diffDays end start

getCandidateDays' :: Integer -> Integer -> Day -> [(Text, Day)]
getCandidateDays' maxnum num day
	| num == 0 = [(pack ("day" ++ (show (maxnum - num))), day)]
	| num > 0  = [(pack ("day" ++ (show (maxnum - num))), day)]
	      	          ++ getCandidateDays' maxnum (num - 1) (addDays 1 day)
	| otherwise = []
