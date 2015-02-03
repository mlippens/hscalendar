module Event
(Recurring(..)
,Event(..)
,toRecurring
,fromEvent
) where
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow
import Control.Applicative
import Safe

data Event = Event
	{ id :: Integer
	, title ::	T.Text
	, authuser :: Integer
	, isRecurring :: Bool
	, recurInterval :: T.Text
	, day :: Integer
	, month :: Integer
	, year :: Integer
	}

data Recurring = 
	None 
	| Daily 
	| Monthly 
	| Yearly deriving(Eq, Enum,Read)

instance Show Recurring where
	show None = "none"
	show Daily = "daily"
	show Monthly = "monthly"
	show Yearly = "yearly"


toRecurring :: T.Text-> Maybe Recurring
toRecurring t = readMay . concat . fmap T.unpack $ [f t,g t]
	where 
		f = T.toUpper . T.singleton . T.head
		g = T.tail

fromEvent :: Integer->Integer->Integer->Event->Event
fromEvent y m d e= Event {Event.id=Event.id e,title=title e,authuser=authuser e,
	isRecurring=isRecurring e,recurInterval=recurInterval e,
	day=d,month=m,year=y}

instance Show Event where
	show (Event id title user recurring interval day month year) = 
		"Event: { id: "++ show id ++
		", title: " ++ T.unpack title ++
		", user: " ++ show user ++
		", isRecurring: " ++ show recurring ++
		", recurInterval: " ++ T.unpack interval ++
		", day: "++ show day ++
		", month: "++ show month ++
		", year: "++ show year ++
		" } \n" 

instance FromRow Event where
	fromRow = Event <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field