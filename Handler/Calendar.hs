module Handler.Calendar where

import Import
import qualified Data.Text as T
import Data.Time
import qualified Data.Time.Calendar as C
import Data.Time.Calendar.MonthDay
import System.Time

data Schedule = Schedule
	{ startDay :: C.Day,
	  endDay :: C.Day
	}
	deriving Show

scheduleAForm :: Html -> MForm Handler (FormResult Schedule, Widget)
scheduleAForm = renderDivs $ Schedule
	<$> areq dayField "Start Day" Nothing
	<*> areq dayField "End Day" Nothing

data Candidates = Candidates 
	{ cdays :: [C.Day]
	}
	deriving Show

candidateAForm :: [(Text, C.Day)] -> Html -> MForm Handler (FormResult Candidates, Widget)
candidateAForm days = renderDivs $ Candidates
	<$> areq (checkboxesFieldList days) "***" Nothing

getCalendarR :: Int -> Int -> Handler Html
getCalendarR year mon = do
	(scheduleWidget, enctype) <- generateFormPost scheduleAForm
	cal' <- liftIO $ getClockTime
	let cal = get1stDayOfSpecifiedMonth mon year cal'
	let isJan = (mon == 1)::Bool
	let isDec = (mon == 12)::Bool
	let nextYear = year + 1
	let prevYear = year - 1
	let nextMon = mon + 1
	let prevMon = mon - 1
	let days = getDays cal
	defaultLayout $(widgetFile "calendar")

postCalendarR :: Int -> Int -> Handler Html
postCalendarR year mon = do
	((result, widget), enctype) <- runFormPost scheduleAForm
	case result of
		FormSuccess schedule -> do
			(scheduleWidget, enctype') <- generateFormPost (candidateAForm days)
			defaultLayout $(widgetFile "schedule")
			where end = endDay schedule
			      start = startDay schedule
			      diff = diffDays end start
			      days = getCandidateDays end start
		_ -> defaultLayout
			[whamlet|
<p>Invalid input, let's try again.
|]

getCandidateDays :: C.Day -> C.Day -> [(T.Text, C.Day)] 
getCandidateDays end start = getCandidateDays' diff diff start
		     	     where diff = diffDays end start

getCandidateDays' :: Integer -> Integer -> C.Day -> [(T.Text, C.Day)]
getCandidateDays' maxnum num day
	| num == 0 = [(T.pack ("day" ++ (show (maxnum - num))), day)]
	| num > 0  = [(T.pack ("day" ++ (show (maxnum - num))), day)]
	      	     	      ++ getCandidateDays' maxnum (num - 1) (addDays 1 day)
	| otherwise = []


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

get1stDayOfMonth :: ClockTime -> CalendarTime
get1stDayOfMonth cal = toUTCTime(addToClockTime
		TimeDiff{tdYear=0, tdMonth=0, tdDay=1-(ctDay (toUTCTime cal)),
		tdHour=0, tdMin=0, tdSec=0, tdPicosec=0} cal)

getMonth :: CalendarTime -> Int
getMonth cal = (fromEnum (ctMonth cal)) + 1

getDays :: CalendarTime -> [Int]
getDays cal = (replicate ((getWeek cal) - 1) 0) ++ [1..len]
		where len = monthLength (isLeapYear (fromIntegral (ctYear cal))) (getMonth cal)

getWeek :: CalendarTime -> Int
getWeek cal = (fromEnum (ctWDay cal)) + 1

