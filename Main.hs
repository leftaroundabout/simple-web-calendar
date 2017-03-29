{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric         #-}


import Yesod
import Yesod.Form.Jquery

import System.Environment (getArgs)

import Data.Text(Text)
import qualified Data.Text as Txt
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as JSON
import Data.Maybe
import Data.List (groupBy, isPrefixOf)
import Data.Ord
import Data.Function (on, (&))
import Data.Char (isAlphaNum)
import qualified Data.Map as Map
import Data.Traversable
import Data.IORef
import Control.Arrow
import Prelude hiding (mapM)
import Control.Monad hiding (mapM, forM)
import Control.Applicative
import Control.Lens
import GHC.Generics

import Data.Thyme
import Data.Thyme.Calendar.WeekDate


type User = Text

type TxtDate = String

data Event = Event {
       eventOrganiser :: User
     , eventDescription :: Text
     } deriving (Generic)
instance FromJSON Event
instance ToJSON Event
data ScheduleForEvent = ScheduleForEvent {
       eventDate :: TxtDate
     , scheduledEvent :: String
     } deriving (Generic)
instance FromJSON ScheduleForEvent

data Permission = Permission {
       _permissionViewAll :: Bool
     , _permissionPostAll :: Bool
     }

data Calendar = Calendar {
    eventsSched :: IORef (Map.Map Day Event)
  , backupFile :: Maybe FilePath
  , userPermissions :: Map.Map User Permission
  }
instance RenderMessage Calendar FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "Calendar" [parseRoutes|
/ HomeR GET
/calendar CalendrR GET POST
/setname SetNameR GET POST
/scheduleevent ScheduleEventR PUT
|]
instance Yesod Calendar
instance YesodJquery Calendar

getHomeR :: Handler Html
getHomeR = redirect CalendrR

getSetNameR :: Handler Html
getSetNameR = defaultLayout $ do
        [whamlet|
         <form class=login method=post>
           <div class=username>
             Name:
             <input type=text name=username>
           <div class=dologin>
             <input type=submit value="enter">
        |]

postSetNameR :: Handler ()
postSetNameR = do
   usr <- runInputPost $ ireq textField "username"
   setSession "username" usr
   redirectUltDest CalendrR
  
getCalendrR :: Handler Html
getCalendrR = do
   sessionUser <- determineUser
   case sessionUser of
     Just thisUser -> do
        Calendar eventsState _ allPermissions <- getYesod
        knownEvents <- liftIO $ readIORef eventsState
        t <- liftIO getCurrentTime
        let today = t^._utctDay
        
        defaultLayout $ do
         setTitle "Calendar"
         toWidget [lucius|body {background-color: grey;}|]
         dispEventCalendr thisUser today knownEvents
     Nothing -> do
        setUltDestCurrent
        redirect SetNameR
       
postCalendrR :: Handler ()
postCalendrR = do
  newEvDay <- read . Txt.unpack <$> runInputPost (ireq textField "day")
  newEvent <- runInputPost $ iopt textField "event"
  tryScheduleEvent newEvDay newEvent

tryScheduleEvent :: Day -> Maybe Text -> Handler ()
tryScheduleEvent newEvDay newEvent = do
  Calendar eventsState _ permission <- getYesod
  oldEvent <- liftIO $ Map.lookup newEvDay <$> readIORef eventsState
  thisUser <- determineUser
  case thisUser of
    Just (usr, Permission _ writeAny) -> case (oldEvent,newEvent) of
        (Nothing, Just new) -> do
            liftIO . modifyIORef eventsState
                     . Map.insert newEvDay $ Event usr new
            updateBackup
        (Just (Event origUsr _), Just new) | origUsr==usr || writeAny -> do
            liftIO . modifyIORef eventsState
                     . Map.insert newEvDay $ Event usr new
            updateBackup
        (Just (Event origUsr _), Nothing) | origUsr==usr || writeAny -> do
            liftIO . modifyIORef eventsState $ Map.delete newEvDay
            updateBackup
        _ -> return ()
    Nothing -> redirect SetNameR
  redirect CalendrR
   
putScheduleEventR :: Handler ()
putScheduleEventR = do
  ScheduleForEvent newEvDaySpec newEvent <- requireJsonBody
  tryScheduleEvent (read newEvDaySpec)
                   (guard (not $ null newEvent) >> pure (Txt.pack newEvent))
   

dispEventCalendr :: (User, Permission) -> Day -> Map.Map Day Event -> Widget
dispEventCalendr usr day₀ events = do
        toWidget [lucius|
               table.calendar, .calendar form input {color: black;}
               .calendar .content .days {table-layout: fixed; width: 100%;}
               .calendar .content .days td {position: relative;}
               .calendar .monthblock #evenmonth {background-color: #88B; color: #55C;}
               .calendar .monthblock #oddmonth {background-color: #8B8; color: #090;}
               .calendar .monthblock .name table {
                  position: relative; width: 5em; }
               .calendar .monthblock .monthname {
                  position: absolute; font-size: 400%; right: 0px;}
               .calendar .monthblock .days .day-in-month {
                  position: absolute; right: 4px; top:2px; font-size: 200%
                ; color: rgba(255,255,255,0.3);}
               .calendar .monthblock .days .event-owner {
                  position: absolute; right: 4px; top:2px; font-size: 50%; color: black; }
               .calendar .monthblock .days .edit-event-day {width: 96%;}
               form #request-day {
                  width: 50%; background-color: rgba(80,80,80,0.5); font-size: 50%;}
               form #event-request {
                  width: 100%; background-color: rgba(160,160,160,0.8);}
               form .event-enter-button {display: none;} |]
        addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
        toWidget [julius|
            $('.edit-event-day form #event-request').blur(function() {
                eventForm = (this).closest('form');
                $.ajax({
                      contentType: "application/json",
                      processData: false,
                      url: "@{ScheduleEventR}",
                      type: "PUT",
                      data: JSON.stringify({
                              eventDate: eventForm.elements["request-day"].value, 
                              scheduledEvent: eventForm.elements["event-request"].value
                            }),
                      dataType: "text"
                  });
            }); |]
        [whamlet|
           <table class=calendar>
             $forall (month,bmonth) <- daysTable
               <tr class=monthblock>
                 <td class=name>
                   <table><tr id=#{monthParity month}><td class=monthname>
                       #{show month}
                 <td class=content>
                   <table class=days>
                     $forall week <- bmonth
                       <tr class=week>
                         $forall day <- week
                          <td class=day id=#{dayMonthParity day}>
                            #{dispDay usr events day}
         |]
 where daysTable = map (\monthblock -> (fst $ head monthblock, snd <$> monthblock))
                 . groupBy ((==)`on`fst)
                 . map (\week -> (last week ^. gregorian . _ymdMonth, week))
                 . groupBy ((==)`on`view (mondayWeek . _mwWeek))
                    $ take 511 [day₀ & mondayWeek . _mwDay .~ 1 ..]
       dayMonthParity d = monthParity $ d ^. gregorian . _ymdMonth
       monthParity m = case m`mod`2 of
                 0 -> "evenmonth" :: Text
                 1 -> "oddmonth"

dispDay :: (User, Permission) -> Map.Map Day Event -> Day -> Html
dispDay (usr, Permission viewAll _) events d = do
  [shamlet| <div class=day-in-month> #{dayInMonth} |]
  case Map.lookup d events of
    Just (Event evUsr ev) -> if viewAll || usr==evUsr
        then [shamlet|
               <div class=edit-event-day>
                 <div .event-owner> #{evUsr}
                 <form method=post>
                  <input #request-day
                         type=text name=day
                         value="#{show d}">
                  <input #event-request
                         type=text name=event
                         value="#{ev}">
                  <input class=event-enter-button
                         type=submit
                         value=enter>
               |]
        else mempty
    Nothing -> dispDay (usr, Permission True False) (Map.singleton d $ Event "" "") d
 where dayId = "day" ++ filter isAlphaNum (show d)
       dayInMonth = d ^. gregorian . _ymdDay


determineUser :: Handler (Maybe (User, Permission))
determineUser = do
    sessionUser <- lookupSession "username"
    Calendar _ _ allPermissions <- getYesod
    return $ case sessionUser of
        Just u -> Just $ case Map.lookup u allPermissions of
                   Just p -> (u, p)
                   Nothing -> (u, Permission False False)
        Nothing -> Nothing

                

updateBackup :: Handler ()
updateBackup = do
    Calendar state bupfile _ <- getYesod
    case bupfile of
      Nothing -> return ()
      Just bup -> liftIO $ do
         calendata <- readIORef state
         BS.writeFile bup . JSON.encode . toJSON $ Map.mapKeys show calendata



main :: IO ()
main = do
   args <- getArgs
   let permissions = parsePermissions args
       bupfile = parseBackupfilename args
   initDates <- newIORef $ Map.empty
   case bupfile of
     Nothing -> do
       putStrLn
         "Warning: running without persistent backup. Data will be lost after shutdown."
     Just bup -> do
       Just (JSON.Success calendata)
               <- fmap JSON.fromJSON . JSON.decode <$> BS.readFile bup
       writeIORef initDates $ Map.mapKeys read calendata
   warp 3735 $ Calendar initDates bupfile permissions


parsePermissions :: [String] -> Map.Map User Permission
parsePermissions [] = Map.empty
parsePermissions (arg:args)
 | "--superuser="`isPrefixOf`arg  = Map.insert
                                      (Txt.pack . filter (/='"') . tail
                                             $ dropWhile (/='=') arg)
                                      (Permission True True)
                                     $ parsePermissions args
 | otherwise  = parsePermissions args

parseBackupfilename :: [String] -> Maybe FilePath
parseBackupfilename [] = Nothing
parseBackupfilename (arg:args)
 | "--backupfile="`isPrefixOf`arg  = Just (filter (/='"') . tail
                                             $ dropWhile (/='=') arg)
 | otherwise  = parseBackupfilename args

