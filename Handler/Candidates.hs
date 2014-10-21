module Handler.Candidates where

import Import

postCandidatesR :: Handler Html
postCandidatesR = do
	mname <- lookupSession "title"
	case mname of 
	        Nothing -> do
			redirect SchedulingR
		Just name -> do
			mday0 <- runInputPost $ iopt textField "day0"
			mday1 <- runInputPost $ iopt textField "day1"
			mday2 <- runInputPost $ iopt textField "day2"
			mday3 <- runInputPost $ iopt textField "day3"
			mday4 <- runInputPost $ iopt textField "day4"
			mday5 <- runInputPost $ iopt textField "day5"
			mday6 <- runInputPost $ iopt textField "day6"
			mday7 <- runInputPost $ iopt textField "day7"
			mday8 <- runInputPost $ iopt textField "day8"
			mday9 <- runInputPost $ iopt textField "day9"
			mday10 <- runInputPost $ iopt textField "day10"
			defaultLayout $(widgetFile "candidates")

