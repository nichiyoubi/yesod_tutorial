module Handler.Calendar where

import Import
import Data.Time
import Data.Time.Calendar.MonthDay
import System.Time

getCalendarR :: String -> Handler Html
getCalendarR text = do
	let mon = read text::Int
	cal' <- liftIO $ getClockTime
	let cal = get1stDayOfSpecifiedMonth mon 2014 cal'
	let days = getDays cal
	defaultLayout $(widgetFile "calendar")

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

