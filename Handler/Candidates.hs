module Handler.Candidates where

import Import

postCandidatesR :: Handler Html
postCandidatesR = do
	textVal0 <- runInputPost $ iopt textField "day0"
	textVal1 <- runInputPost $ iopt textField "day1"
	textVal2 <- runInputPost $ iopt textField "day2"
	textVal3 <- runInputPost $ iopt textField "day3"
	textVal4 <- runInputPost $ iopt textField "day4"
	textVal5 <- runInputPost $ iopt textField "day5"
	textVal6 <- runInputPost $ iopt textField "day6"
	textVal7 <- runInputPost $ iopt textField "day7"
	textVal8 <- runInputPost $ iopt textField "day8"
	textVal9 <- runInputPost $ iopt textField "day9"
	textVal10 <- runInputPost $ iopt textField "day10"
	defaultLayout $ do
		[whamlet|
<p>Invalid input, let's try again.
#{show textVal1} #{show textVal2}
<hr>
<div id="calendar">
  <div class="back">
    <a href=../cal/2014/03>back
|]

