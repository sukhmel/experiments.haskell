{-
 - Watch.hs - a simple countdown clock using wxHaskell.
 -
 - Author: William Tan <wil@dready.org>
 - Accompanying presentation:
 -   http://dready.org/papers/wxHaskell/GUIProg_wxHaskell.pdf
 -
 - Commandline arguments: [-s] [<interval>]
 -   If "-s" is specified, the timer will start immediately
 -   An <interval> can be specified using the [[HH:]mm:]ss format.
 -   The default interval is 5 minutes (defInterval)
 -}


module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore hiding(Timer)
import Time
import Monad
import System



-- default interval = 5 minutes
defInterval = 5 * 60


-- getTimeString: returns a formatted string representing the current time
getTimeString :: IO String
getTimeString = do
	now <- getClockTime
	calTime <- toCalendarTime now
	let (CalendarTime {ctHour=hour, ctMin=minute, ctSec=second}) = calTime

	let minStr =
		if minute <= 9 then "0" ++ show minute
		else show minute

	let secStr =
		if second <= 9 then "0" ++ show second
		else show second

	return $ show hour ++ ":" ++ minStr ++ ":" ++ secStr


-- parseTime: given a string representation, returns the time in seconds
parseTime :: String -> Int
parseTime s = case reads s of
	[]      -> 0
	(t1,[]):[] -> t1
	(t1,(':':s1)):[] -> case reads s1 of
				(t2,[]):[] -> t1*60 + t2
				(t2,(':':s2)):[] -> t1*3600 + t2*60 + read s2


-- splitTime: split the time into (hr,min,sec)
splitTime :: Int -> (Int, Int, Int)
splitTime tm = (hr, min, sec)
	where	sec = tm `mod` 60
		min = ((tm-sec) `div` 60) `mod` 60
		hr  = (((tm-sec) `div` 60)-min) `div` 60


-- getInterval: get the corresponding 
getInterval :: SpinCtrl a -> SpinCtrl a -> SpinCtrl a -> IO Int
getInterval h m s = do
	secs <- spinCtrlGetValue s
	mins <- spinCtrlGetValue m
	hrs  <- spinCtrlGetValue h
	return ((secs + mins*60 + hrs*60*60) * 1000)


-- the action to perform when an alarm sounds
alarm :: Window a -> IO ()
alarm win = do
	windowRaise win
	wxcAppSetTopWindow win -- be ultra irritating
	infoDialog win "Alarm" "Time's up!"


-- kick start the UI
uiMain :: Int -> Bool -> IO ()
uiMain intv startNow = do
	-- create main window
	f <- frame [text := "Countdown Watch", clientSize := sz 300 200]

	-- a panel to contain the controls
	panel <- panel f []

	-- create menus
	timerMenu <- menuPane		[text := "&Timer"]
	tstart    <- menuItem timerMenu [text := "&Start"]
	quit      <- menuQuit timerMenu [help := "Quit"]

	-- labels
	timeLabel <- staticText panel [text := "Time: ", fontWeight := WeightBold]
	intvLabel <- staticText panel [text := "Interval: ", fontWeight := WeightBold]
	-- static text for displaying current time
	timeStr <- getTimeString
	timeStatic <- staticText panel [text := timeStr]

	let (hrVal, minVal, secVal) = splitTime intv

	-- the spin controls for setting alarm interval
	hr  <- spinCtrl panel 0 99 [outerSize := sz 35 20]
	min <- spinCtrl panel 0 59 [outerSize := sz 35 20]
	sec <- spinCtrl panel 0 59 [outerSize := sz 35 20]
	spinCtrlSetValue hr hrVal
	spinCtrlSetValue min minVal
	spinCtrlSetValue sec secVal

	-- start/cancel button
	startBtn  <- button panel [text := "Start"]
	set startBtn [on command := setAlarm f startBtn hr min sec]

	-- set the alarm now, if instructed on command line
	Monad.when startNow $ setAlarm f startBtn hr min sec

	-- place the panel onto the frame
	set f     [menubar := [timerMenu]
--	          ,layout  := fill $ column 1 $ [hfill $ hrule 1, fill $ widget panel]
	          ,on (menu quit) := close f
	          ]

	set f [layout :=
		column 1 $ [
			hfill $ hrule 1,
			fill $ container panel $
				margin 10 $ column 10 [
					hfill $ row 1 [widget timeLabel, glue, widget timeStatic],
					hfill $ row 1 [widget intvLabel, glue, widget hr, label ":", widget min, label ":", widget sec],
					floatBottomRight $ widget startBtn]]]

	-- create a timer to update the clock
	timerClk <- timer f [ on command := do { t <- getTimeString; set timeStatic [text := t]; windowRefresh timeStatic False} ]
	return ()


-- setAlarm: schedule a timer and update UI
setAlarm :: Frame a -> Button a -> SpinCtrl a -> SpinCtrl a -> SpinCtrl a -> IO ()
setAlarm frm btn hr min sec = do
	set btn [enabled := False] -- disable button temporarily
	numsecs <- getInterval hr min sec
	timer <- timer frm [interval := numsecs]
	set timer [on command := do { disableAlarm timer btn $ setAlarm frm btn hr min sec; alarm frm }]
	-- change the command handler to cancel timer
	-- and restore the original handler
	set btn [text := "Cancel"]
	set btn [on command :~ \prev -> do { set timer [enabled := False]; set btn [text := "Start", on command := prev]}]
	set btn [enabled := True] -- reenable button


-- disableAlarm: timer
disableAlarm :: Timer -> Button a -> IO () -> IO ()
disableAlarm timer btn setAlarmAction = do
	set timer [enabled := False]
	set btn   [text := "Start"]
	set btn   [on command := setAlarmAction]


-- program entry point
main :: IO ()
main  = do
	args <- System.getArgs
	let intv = if length args > 0 then parseTime (head args) else defInterval
	let (startNow, intv) = case args of
		[] -> (False, defInterval)
		["-s"] -> (True, defInterval)
		[time] -> (False, parseTime time)
		["-s", time] -> (True, parseTime time)
		_  -> error "watch [-s] [time]"
	start $ uiMain intv startNow
