{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BT
import           Data.Text.Encoding
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import           System.Time
import           Safe
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as C
import qualified Text.XmlHtml as X
import qualified Event as E
import Control.Monad.IO.Class
import Data.Monoid
import Control.Monad
import Data.List
import Data.Ord
import Data.Maybe
import Safe
import Database.PostgreSQL.Simple.FromRow
import qualified DateHelper as DH
------------------------------------------------------------------------------
import           Application


type Rows = [[String]]
type Row = [String]


calendarURI :: MonadIO m => m ByteString
calendarURI = liftM ((BT.append "/calendar/") . encodeUtf8 . convertToURI . (\(y,m)-> DH.getMonthRepresentation y m)) $ liftIO DH.currentDateTuple 
      
convertToURI :: DH.CalendarMonth -> T.Text
convertToURI calmonth =
  T.pack $ show year ++ "/" ++ show month
  where 
    year = DH.year calmonth
    month = DH.month calmonth

getUid :: Maybe AuthUser -> Maybe Integer
getUid (Just user) =  maybe Nothing (readMay . T.unpack . unUid) $ userId user
getUid _ = Nothing

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = do
    successUri <- calendarURI
    loginUser "login" "password" Nothing
              (\_ ->handleLogin err) (redirect successUri)
  where
    err = Just "Unknown user or password"

createEvent :: (Integer,Integer,Integer) -> ByteString -> E.Recurring -> Handler App App ()
createEvent (d,m,y) eventname recur = do
  uid <- getUid <$> (with auth $ currentUser)
  let recurring = recur /= E.None
  case uid of
    (Just id)-> do
      execute "INSERT INTO events(title,authuser,isrecurring,recurinterval,day,month,year) VALUES(?,?,?,?,?,?,?);" 
        (eventname,id,recurring, show recur,d,m,y)
      return ()
    _ -> return ()

deleteEvent :: Integer -> Handler App App ()
deleteEvent id = do
  execute "DELETE FROM events WHERE id = ?" (Only id)
  return ()

getEvents :: Integer -> Integer -> Handler App App ([E.Event])
getEvents year month= do
  let querystring = "select \
    \ id,title,authuser,isrecurring,recurinterval::text,day,month,year \
    \ from events where year= ? and month= ? \
    \ order by day "
  events <- query querystring  (year,month)
  return (events :: [E.Event])

getRecurringEvents :: Integer -> Integer -> Handler App App([E.Event])
getRecurringEvents y m = do
  let querystring = "select \
    \ id,title,authuser,isrecurring,recurinterval::text,day,month,year \
    \ from events \
    \ where isrecurring=true and year <= ? and month <= ?"
  events <- query querystring (y,m)
  return $ DH.getRecurringEvents (y,m) events


handleDeleteEvent :: Handler App App ()
handleDeleteEvent = do
  id <- liftM (>>= (readMay . BT.unpack)) $ getParam "id"
  case id of
    (Just id)-> deleteEvent id
    Nothing-> render "error"
  handleGetEvents

handleEvents :: Handler App App()
handleEvents = method GET handleGetEvents <|> method POST handlePostEvent

handleGetEvents :: Handler App App ()
handleGetEvents = do
  year  <- liftM (>>= (readMay . BT.unpack)) $ getParam "year"
  month <- liftM (>>= (readMay . BT.unpack)) $ getParam "month"
  case sequence [year,month] of
    Just [y,m]-> do
      let calendar = DH.getMonthRepresentation y m
      heistLocal (I.bindSplices $ calendarSplices calendar) $ render "event" 
    _ -> do
      uri <- calendarURI
      redirect uri

handlePostEvent :: Handler App App ()
handlePostEvent = do
  name <- maybe "No event Name" id <$> getPostParam "eventName"
  recurring <- maybe E.None (toEnum . read . BT.unpack) <$> getPostParam "recurring"
  day <-  liftM (>>= readMay . BT.unpack) $ getPostParam "day"
  month <-liftM (>>= readMay . BT.unpack) $ getPostParam "month"
  year <- liftM (>>= readMay . BT.unpack) $ getPostParam "year"
  case sequence [day,month,year] of
    Just [d,m,y] -> do
      createEvent (d,m,y) name recurring
      redirect (BT.append "/calendar/" . encodeUtf8 . convertToURI $ DH.getMonthRepresentation y m) 
    _ -> render "error"


handleCalendar :: Handler App App ()
handleCalendar = do 
  year  <- liftM (>>= (readMay . BT.unpack)) $ getParam "year"
  month <- liftM (>>= (readMay . BT.unpack)) $ getParam "month"
  case sequence [year,month] of
    Just [y,m] -> do
      events <- getEvents y m
      recurringEvents <- getRecurringEvents y m
      uid <- getUid <$> (with auth $ currentUser)
      let calendar = DH.getMonthRepresentation y m
      heistLocal (I.bindSplices $ splice calendar (sortBy (comparing E.day) $ recurringEvents `mappend` events) uid) $ render "calendar"
      where 
        splice cal events uid = calendarSplices cal >> ("events" ## (renderEvents uid events))
    _ -> render "error"
calendarSplices :: DH.CalendarMonth -> Splices (I.Splice AppHandler)
calendarSplices cal = do
  "calendar"      ## (renderCalendar cal)
  "month"         ## I.textSplice . T.pack . show . DH.month $ cal
  "monthAsString" ## I.textSplice . T.pack . DH.toMonthString . DH.month $ cal
  "year"          ## I.textSplice . T.pack . show . DH.year $ cal
  "next"          ## I.textSplice . convertToURI . DH.addMonths 1 $ cal
  "previous"      ## I.textSplice . convertToURI . DH.addMonths (-1) $ cal
  "nextyear"      ## I.textSplice . convertToURI . DH.addYears 1 $ cal
  "previousyear"  ## I.textSplice . convertToURI . DH.addYears (-1) $ cal


renderEvents :: Maybe Integer -> [E.Event] -> SnapletISplice App
renderEvents uid e = I.mapSplices (I.runChildrenWith . eventSplice uid) e

data Hole = Hole

eventSplice :: Monad n => Maybe Integer -> E.Event -> Splices (I.Splice n)
eventSplice uid e = do
  "id" ## I.textSplice . T.pack . show $ E.id e
  "title" ## I.textSplice $ E.title e
  "recurs" ## I.textSplice . T.pack . show $ E.isRecurring e
  "interval" ## I.textSplice $ E.recurInterval e 
  "day" ## I.textSplice . T.pack . show $ E.day e
  "month" ## I.textSplice . T.pack . show $ E.month e
  "year" ## I.textSplice . T.pack . show $ E.year e
  "monthAsString" ## I.textSplice . T.pack . DH.toMonthString $ E.month e
  case uid of
    (Just id)-> case id == (E.authuser e) of
      True->"delete" ## I.runChildren
      _ ->  errorSplice
    _ -> errorSplice
  where
    errorSplice = "delete" ## I.textSplice "you can't delete this event!"

renderCalendar :: DH.CalendarMonth -> SnapletISplice App
renderCalendar cal = I.runChildrenWith splicefuncs
  where 
    splicefuncs = do
      "rows" ## rowSplice $ DH.rows cal

rowSplice :: Rows -> SnapletISplice App
rowSplice r = I.mapSplices (I.runChildrenWith . dataSplice) r

dataSplice :: Monad n => Row -> Splices (I.Splice n)
dataSplice r = do
  "row" ## I.mapSplices (I.runChildrenWith . valueSplice) r

valueSplice :: Monad n => String -> Splices (I.Splice n)
valueSplice r = "value" ## I.textSplice $ T.pack r
 

------------------------------------------------------------------------------
-- | Logs out and redirdects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = do 
      result <- registerUser "login" "password" 
      case result of
        Left e    -> handleNewUserError $ Just e
        _ -> do 
          uri <- calendarURI
          redirect uri

handleNewUserError :: Maybe AuthFailure -> Handler App (AuthManager App)()
handleNewUserError authError = heistLocal (I.bindSplices errs) $ render "new_user"
  where
    errs = maybe noSplices splice authError
    splice e = "registerError" ## I.textSplice $  T.pack $ show e

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/calendar/:year/:month", handleCalendar)
         , ("/calendar/delete/:id",handleDeleteEvent)
         , ("/event/:year/:month", handleEvents)
         , ("",          serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    pg <- nestSnaplet "pg" pg pgsInit

    a <- nestSnaplet "auth" auth $ initPostgresAuth sess pg
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a pg
