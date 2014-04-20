module Handler.Calendar where

import Import
import Data.Time
import Data.Time.Calendar.MonthDay
import System.Time

getCalendarR :: String -> Handler Html
getCalendarR text = do
	cal  <- liftIO $ getClockTime
	cal' <- liftIO $ toCalendarTime cal
	defaultLayout $ do
		setTitle "Calendar"
		[whamlet|<h1>#{show (getMonth cal')}<h2>#{show (getDays cal')}|]

getCalendar :: String
getCalendar = "test"

getMonth :: CalendarTime -> Int
getMonth cal = (fromEnum (ctMonth cal)) + 1

getDays :: CalendarTime -> [Int]
getDays cal = (replicate ((getWeek cal) - 1) 0) ++ [1..length]
		where length = monthLength (isLeapYear (fromIntegral (ctYear cal))) (getMonth cal)

getWeek :: CalendarTime -> Int
getWeek cal = (fromEnum (ctWDay cal)) + 1

