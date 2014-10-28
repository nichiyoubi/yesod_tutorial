module Handler.Candidates where

import Import

postCandidatesR :: Handler Html
postCandidatesR = do
	mname <- lookupSession "title"
	case mname of 
	        Nothing -> do
			redirect SchedulingR
		Just name -> do
		        titles <- runDB $ selectList [] [Asc CandidateDaysTitle]
--			id <- runDB $ insert $ CandidateDays name []

			mday0 <- runInputPost $ iopt textField "day0"
			case mday0 of Just day0 -> (runDB $ insert $ CandidateDays name day0)
			mday1 <- runInputPost $ iopt textField "day1"
			case mday1 of Just day1 -> (runDB $ insert $ CandidateDays name day1)
			mday2 <- runInputPost $ iopt textField "day2"
			case mday2 of Just day2 -> (runDB $ insert $ CandidateDays name day2)
			mday3 <- runInputPost $ iopt textField "day3"
			case mday3 of Just day3 -> (runDB $ insert $ CandidateDays name day3)
			mday4 <- runInputPost $ iopt textField "day4"
			case mday4 of Just day4 -> (runDB $ insert $ CandidateDays name day4)
			mday5 <- runInputPost $ iopt textField "day5"
			case mday5 of Just day5 -> (runDB $ insert $ CandidateDays name day5)
			mday6 <- runInputPost $ iopt textField "day6"
			case mday6 of Just day6 -> (runDB $ insert $ CandidateDays name day6)
			mday7 <- runInputPost $ iopt textField "day7"
			case mday7 of Just day7 -> (runDB $ insert $ CandidateDays name day7)
			mday8 <- runInputPost $ iopt textField "day8"
			case mday8 of Just day8 -> (runDB $ insert $ CandidateDays name day8)
			mday9 <- runInputPost $ iopt textField "day9"
			case mday9 of Just day9 -> (runDB $ insert $ CandidateDays name day9)
			mday10 <- runInputPost $ iopt textField "day10"
			case mday10 of Just day10 -> (runDB $ insert $ CandidateDays name day10)
			defaultLayout $(widgetFile "candidates")

