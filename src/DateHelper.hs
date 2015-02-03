module DateHelper 
(getMonthRepresentation
,toMonthString
,addMonths
,addYears
,getRecurringEvents
,currentDate
,currentDateTuple
,CalendarMonth (..)) where

import System.Time
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.WeekDate
import Data.List.Split
import Control.Monad
import Control.Applicative
import qualified Data.Text as T
import qualified Event as E

data CalendarMonth = CalendarMonth {month :: Integer,year :: Integer, rows :: [[String]]} deriving(Show)
 
currentDate :: IO Data.Time.Calendar.Day
currentDate = getCurrentTime >>= return . utctDay 

currentDateTuple :: IO (Integer,Integer)
currentDateTuple = do
	(year,month,_)<- toGregorian <$> currentDate
	return (year,fromIntegral $ month)

getRecurringEvents :: (Integer,Integer) -> [E.Event] -> [E.Event]
getRecurringEvents info events = concat $ fmap g events
	where g e = case E.toRecurring . E.recurInterval $ e of
		Nothing -> []
		(Just E.None)-> []
		(Just E.Daily)-> fDaily info e
		(Just E.Monthly)-> fMonthly info e
		(Just E.Yearly)-> fYearly info e


fDaily :: (Integer,Integer) -> E.Event -> [E.Event]
fDaily (year,month) e = case month > E.month e of
	True -> fmap eventFromYearMonth [1..monthLength]
	False -> fmap eventFromYearMonth [currentDay+1..monthLength]
	where 
		monthLength = fromIntegral . fst $ monthInfo year month
		eventFromYearMonth = (flip $ E.fromEvent year month) $ e
		currentDay = E.day e



fMonthly :: (Integer,Integer) -> E.Event -> [E.Event]
fMonthly (year,month) e = case month > E.month e of
	True -> [E.fromEvent year month calculatedDay e]
		where calculatedDay = getDay $ until (both ((==month) . getMonth) ((==year) . getYear)) (addGregorianMonthsClip 1) $ eventToGregorian e
			
	_ -> []	

fYearly :: (Integer,Integer) -> E.Event -> [E.Event]
fYearly (year,month) e = case year == E.year e of
	False-> case month == E.month e of
		True-> [E.fromEvent year month calculatedDay e]
		_-> []
	_-> []
	where 
		calculatedDay = getDay $ until ((==year) . getYear) (addGregorianYearsClip 1) $ eventToGregorian e



getMonth :: Data.Time.Calendar.Day -> Integer
getMonth = toInteger . (\(_,m,_)-> m) . toGregorian

getYear :: Data.Time.Calendar.Day -> Integer
getYear = toInteger . (\(y,_,_)-> y) . toGregorian

getDay :: Data.Time.Calendar.Day -> Integer
getDay = toInteger . (\(_,_,d)-> d) . toGregorian 

both :: (a->Bool)->(a->Bool)->a->Bool
both f g e = f e && g e

eventToGregorian :: E.Event ->Data.Time.Calendar.Day
eventToGregorian e = (fromGregorian (E.year e) (fromIntegral $ E.month e) (fromIntegral $ E.day e)) 


monthInfo :: Integer -> Integer -> (Int,Int)
monthInfo year month = let 
	length 	= gregorianMonthLength year $ fromIntegral month
	fst		= fromGregorian year (fromIntegral month) 1
	(_,_,day) 	= toWeekDate fst
	in (length,day)

toMonthString :: Integer -> String
toMonthString m = show $ (toEnum ((fromIntegral m) -1) :: Month)


addMonths :: Integer -> CalendarMonth -> CalendarMonth
addMonths amount (CalendarMonth {year=y,month=m}) =
	let (newyear,newmonth,_) = toGregorian . addGregorianMonthsClip amount $ fromGregorian y (fromIntegral m) 1
	in getMonthRepresentation newyear $ toInteger newmonth

addYears :: Integer -> CalendarMonth -> CalendarMonth
addYears amount (CalendarMonth {year=y,month=m}) =
	let (newyear,newmonth,_) = toGregorian . addGregorianYearsClip amount $ fromGregorian y (fromIntegral m) 1
	in getMonthRepresentation newyear $ toInteger newmonth


getMonthRepresentation :: Integer -> Integer -> CalendarMonth
getMonthRepresentation year month = let 
	(len,day) = monthInfo year month
	sunday = 7-day+1
	firstRow = replicate (day-1) "" ++ fmap show [1..sunday] 
	rows = fmap (\lst-> let len = length lst in if len == 7 then lst else lst ++ (replicate (7-len) "")) $ chunksOf 7 $ fmap show [sunday+1..len]
	in CalendarMonth month year ([firstRow] ++ rows)
