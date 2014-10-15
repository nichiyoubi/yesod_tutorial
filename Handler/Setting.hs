module Handler.Setting where

import Import
import Data.Time
import qualified Data.Time.Calendar as C
import Data.Time.Calendar.MonthDay
import System.Time
import Handler.Scheduling

data Setting = Setting
        { startDay :: C.Day,
          endDay :: C.Day
	}
	deriving Show

settingForm :: Html -> MForm Handler (FormResult Setting, Widget)
settingForm = renderDivs $ Setting
        <$> areq dayField "開始日" Nothing
	<*> areq dayField "終了日" Nothing

getSettingR :: Int -> Int -> Handler Html
getSettingR year mon = do
	(settingWidget, enctype) <- generateFormPost settingForm
	mname <- lookupSession "title"
	case mname of
	        Nothing -> do
		        redirect SchedulingR
		Just name -> do
			cal' <- liftIO $ getClockTime
			let cal = get1stDayOfSpecifiedMonth mon year cal'
			let isJan = (mon == 1)::Bool
			let isDec = (mon == 12)::Bool
			let nextYear = year + 1
			let prevYear = year - 1
			let nextMon = mon + 1
			let prevMon = mon - 1
			let days = getDays cal
			defaultLayout $(widgetFile "setting")

postSettingR :: Int -> Int -> Handler Html
postSettingR year mon = do
        ((result, widget), enctype) <- runFormPost schedulingForm
	case result of
	        FormSuccess schedule -> do
			let name = title schedule
			setSession "title" name
			(settingWidget, enctype') <- generateFormPost settingForm
			cal' <- liftIO $ getClockTime
			let cal = get1stDayOfSpecifiedMonth mon year cal'
			let isJan = (mon == 1)::Bool
			let isDec = (mon == 12)::Bool
			let nextYear = year + 1
			let prevYear = year - 1
			let nextMon = mon + 1
			let prevMon = mon - 1
			let days = getDays cal
			defaultLayout $(widgetFile "setting")
		_ -> defaultLayout
		        [whamlet|
<p>Invalid input, let's try agein.
<a href=@{SchedulingR}>back
|]   

get1stDayOfSpecifiedMonth :: Int -> Int -> ClockTime -> CalendarTime
get1stDayOfSpecifiedMonth mon year cal = toUTCTime(addToClockTime
		TimeDiff{tdYear=year-(ctYear (toUTCTime cal)),
			 tdMonth=mon-(getMonth (toUTCTime cal)),
			 tdDay=1-(ctDay (toUTCTime cal)),
			 tdHour=0, tdMin=0, tdSec=0, tdPicosec=0} cal)

getCalendarString :: [Int] -> [String]
getCalendarString [] = []
getCalendarString [x]
	| x == 0 = ["_"]
	| x /= 0 = [show x]
getCalendarString (x:xs)
	| x == 0 = ["_"] ++ getCalendarString xs
	| x /= 0 = [show x] ++ getCalendarString xs
getCalendarString [_] = []
getCalendarString (_:_) = []

getMonth :: CalendarTime -> Int
getMonth cal = (fromEnum (ctMonth cal)) + 1

getDays :: CalendarTime -> [Int]
getDays cal = (replicate ((getWeek cal) - 1) 0) ++ [1..len]
		where len = monthLength (isLeapYear (fromIntegral (ctYear cal))) (getMonth cal)

getWeek :: CalendarTime -> Int
getWeek cal = (fromEnum (ctWDay cal)) + 1
