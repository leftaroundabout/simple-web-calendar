{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric         #-}


import Yesod
import Yesod.Form.Jquery

import Text.Cassius (Css, renderCss)
import Text.Julius (Javascript)

import System.Environment (getArgs)

import Data.Text(Text)
import qualified Data.Text as Txt
import qualified Data.Text.Lazy as Txt (toStrict)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import Data.Maybe
import Data.List (groupBy, isPrefixOf, intercalate)
import Data.Monoid
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
import Lens.Micro
import GHC.Generics

import Control.Concurrent
import Network.Mail.Mime
import Network.Mail.Account
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as Blaze

import Data.Thyme
import Data.Thyme.Calendar.WeekDate


type UserName = Text
type MailAdress = Text

type TxtDate = String

data Event = Event {
       eventOrganiser :: UserName
     , eventDescription :: Text
     } deriving (Generic, Show)
instance FromJSON Event
instance ToJSON Event
data ScheduleForEvent = ScheduleForEvent {
       eventDate :: TxtDate
     , scheduledEvent :: String
     } deriving (Generic)
instance FromJSON ScheduleForEvent

data Actor = Actor {
      actorPermissions :: Permission
    , actorSubscribers :: [MailAdress]
    } deriving (Generic)
instance FromJSON Actor
instance ToJSON Actor
data Permission = Permission {
       permissionViewAll :: Bool
     , permissionPostAll :: Bool
     } deriving (Generic)
instance FromJSON Permission
instance ToJSON Permission

type UsersConfiguration = Map.Map UserName Actor

data GlobalConfig = GlobalConfig {
      usersConfiguration :: UsersConfiguration
    , notifierAccounts :: [MailAccount]
    , backupFiles :: [FilePath]
    , tcpPortNumber :: Maybe Int
    } deriving (Generic)
instance FromJSON GlobalConfig
instance ToJSON GlobalConfig

data Calendar = Calendar {
    eventsSched, freshlyChanged :: IORef (Map.Map Day Event)
  , globalConfig :: GlobalConfig
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
getSetNameR = do
   clearSession
   defaultLayout $ do
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
   liftIO $ putStrLn ("User " ++ show usr ++ " logged in.")
   redirectUltDest CalendrR
  
getCalendrR :: Handler Html
getCalendrR = do
   sessionUser <- determineUser
   case sessionUser of
     Just thisUser -> do
        Calendar eventsState _ (GlobalConfig allPermissions _ _ _) <- getYesod
        knownEvents <- liftIO $ readIORef eventsState
        t <- liftIO getCurrentTime
        let today = t^._utctDay
        
        defaultLayout $ do
         setTitle "Calendar"
         toWidget [lucius|body {background-color: grey;}
                          #usr-status {text-align: right; color: black;}|]
         [whamlet| <p #usr-status>
                      #{fst thisUser}
                      <a href=@{SetNameR}> logout |]
         toWidget $ dispEventCalendr thisUser today knownEvents
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
  Calendar allEvents recentSched (GlobalConfig permission _ _ _) <- getYesod
  thisUser <- determineUser
  let modifyTarget inj postHook eventsState
       = liftIO (Map.lookup newEvDay <$> readIORef eventsState)
         >>= \oldEvent -> case thisUser of
       Just (usr, Permission _ writeAny) -> case (oldEvent, inj $ Event usr <$> newEvent) of
        (Nothing, Just new) -> do
            liftIO . modifyIORef eventsState $ Map.insert newEvDay new
            postHook
        (Just (Event origUsr _), Just new) | origUsr==usr || writeAny -> do
            liftIO . modifyIORef eventsState $ Map.insert newEvDay new
            postHook
        (Just (Event origUsr _), Nothing) | origUsr==usr || writeAny -> do
            liftIO . modifyIORef eventsState $ Map.delete newEvDay
            postHook
        _ -> return ()
       Nothing -> redirect SetNameR
  modifyTarget id updateBackup allEvents
  modifyTarget (\case {Nothing | Just (usr,_)<-thisUser -> Just $ Event usr ""; e->e})
                  (return()) recentSched
  redirect CalendrR
   
putScheduleEventR :: Handler ()
putScheduleEventR = do
  ScheduleForEvent newEvDaySpec newEvent <- requireJsonBody
  tryScheduleEvent (read newEvDaySpec)
                   (guard (not $ null newEvent) >> pure (Txt.pack newEvent))
   

dispEventCalendr :: (UserName, Permission) -> Day -> Map.Map Day Event -> PseudoWidget
dispEventCalendr usr day₀ events = PseudoWidget {
     widgetCSS = [lucius|
               table.calendar, .calendar form input {color: black;}
               .calendar .content .days {table-layout: fixed; width: 100%;}
               .calendar .content .days td {position: relative;}
               .calendar .monthblock #evenmonth {background-color: #88B; color: #55C;}
               .calendar .monthblock #oddmonth {background-color: #8B8; color: #090;}
               .calendar .monthblock #weekend {background-color: #B88; color: #090;}
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
               form .event-enter-button {display: none;} |] ()
   , widgetJS = [julius|
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
   , widgetHTML = [shamlet|
          <p #main-window>
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
   }
 where daysTable = map (\monthblock -> (fst $ head monthblock, snd <$> monthblock))
                 . groupBy ((==)`on`fst)
                 . map (\week -> (last week ^. gregorian . _ymdMonth, week))
                 . groupBy ((==)`on`(^. mondayWeek . _mwWeek))
                    $ take 511 [day₀ & mondayWeek . _mwDay .~ 1 ..]
       dayMonthParity d
           | d ^. mondayWeek . _mwDay < 6
                = monthParity $ d ^. gregorian . _ymdMonth
           | otherwise  = "weekend"
       monthParity m = case m`mod`2 of
                 0 -> "evenmonth" :: Text
                 1 -> "oddmonth"

dispDay :: (UserName, Permission) -> Map.Map Day Event -> Day -> Html
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


determineUser :: Handler (Maybe (UserName, Permission))
determineUser = do
    sessionUser <- lookupSession "username"
    Calendar _ _ (GlobalConfig allUsers _ _ _) <- getYesod
    return $ case sessionUser of
        Just u | not $ Txt.null u
              -> Just $ case Map.lookup u allUsers of
                   Just (Actor p _) -> (u, p)
                   Nothing -> (u, Permission False False)
        Nothing -> Nothing

                

updateBackup :: Handler ()
updateBackup = do
    Calendar state _ (GlobalConfig _ _ bupfiles _) <- getYesod
    forM_ bupfiles $ \bup -> liftIO $ do
         calendata <- readIORef state
         BS.writeFile bup . JSON.encode $ Map.mapKeys show calendata


instance Monoid GlobalConfig where
  mempty = GlobalConfig Map.empty [] [] Nothing
  mappend (GlobalConfig u₀ n₀ b₀ p₀) (GlobalConfig u₁ n₁ b₁ p₁)
       = GlobalConfig (Map.unionWith updateActor u₀ u₁) (n₀<>n₁) (b₀<>b₁) (p₀<|>p₁)
   where updateActor (Actor (Permission view₀ post₀) subs₀)
                     (Actor (Permission view₁ post₁) subs₁)
                  = Actor (Permission (view₀||view₁) (post₀||post₁)) (subs₀<>subs₁)


notifier :: MailAccount -> Calendar -> IO ()
notifier account (Calendar allCal news (GlobalConfig users permissions _ _)) = loop
 where loop = do
        threadDelay $ 8 * 10^6
        toAnnounce <- atomicModifyIORef news $ \n -> (Map.empty, Map.toList n)
        calendata <- readIORef allCal
        forM_ (Map.toList users) $ \(usrName, Actor permissions subs) -> do
          let relevantEvents = filter (relevant . snd) toAnnounce
              relevant (Event organiser _)
                   = permissionViewAll permissions || organiser==usrName
              subject = show relevantEvents
              userCalendar = Map.filter relevant calendata
              (soonest, _) = Map.findMin userCalendar
--          mailCalendar <- 
          when (not $ null relevantEvents) . forM_ subs $ \subscriber -> do
            putStrLn subject
            account `sendsMail` \m -> m
             { mailTo = [Address Nothing subscriber]
             , mailHeaders
                = [("Subject", Txt.pack subject)]
             , mailParts
                = [[Part
                    { partType = "text/html; charset=utf-8"
                    , partEncoding = None
                    , partFilename = Nothing
                    , partContent = renderHtml . flatWidget
                            $ dispEventCalendr (usrName, permissions) soonest userCalendar
                    , partHeaders = []
                    }]
                  ,[Part
                    { partType = "text/json; charset=utf-8"
                    , partEncoding = None
                    , partFilename = Just "calendar.json"
                    , partContent = JSON.encodePretty $ Map.mapKeys show userCalendar
                    , partHeaders = []
                    }]
                  ]
             }
        loop



main :: IO ()
main = do
   args <- getArgs
   
   config <- parseConfiguration args
   dispConfigurationInfo config
   
   initDates <- newIORef $ Map.empty
   nothingRecent <- newIORef $ Map.empty
   case backupFiles config of
     [] -> do
       putStrLn
         "Warning: running without persistent backup. Data will be lost after shutdown."
     (bup:bups) -> do
       Just (JSON.Success calendata)
               <- fmap JSON.fromJSON . JSON.decode <$> BS.readFile bup
       writeIORef initDates $ Map.mapKeys read calendata
   let calendar = Calendar initDates nothingRecent config
   forM_ (notifierAccounts config) $ \account -> forkIO $ notifier account calendar
   warp (maybe 3735 id $ tcpPortNumber config) calendar



parseConfiguration :: [String] -> IO GlobalConfig
parseConfiguration [] = return mempty
parseConfiguration (arg:args)
 | "--config-file="`isPrefixOf`arg = do
        let confFile = (filter (/='"') . tail $ dropWhile (/='=') arg)
        Just (JSON.Success thisConfig)
               <- fmap JSON.fromJSON . JSON.decode <$> BS.readFile confFile
        (thisConfig<>) <$> parseConfiguration args
 | "--backupfile="`isPrefixOf`arg
 , bupfile <- (filter (/='"') . tail $ dropWhile (/='=') arg)
         = (GlobalConfig mempty [] [bupfile] Nothing<>) <$> parseConfiguration args
 | "--superuser="`isPrefixOf`arg
 , actor <- (Txt.pack . filter (/='"') . tail $ dropWhile (/='=') arg)
         = (GlobalConfig (Map.singleton actor (Actor (Permission True True) []))
                             [] [] Nothing<>) <$> parseConfiguration args
 | otherwise  = parseConfiguration args


dispConfigurationInfo :: GlobalConfig -> IO ()
dispConfigurationInfo conf@(GlobalConfig _ notifierAccounts _ _) = do
        putStrLn $ "Sending change notifications from "
                     ++ (intercalate ", " $ show . userMail <$> notifierAccounts)
        when False . BS.writeFile "global-config.json" $ JSON.encodePretty conf



data PseudoWidget = PseudoWidget {
     widgetHTML :: Html
   , widgetCSS :: Css
   , widgetJS :: JavascriptUrl (Route Calendar)
   }

instance ToWidget Calendar PseudoWidget where
  toWidget (PseudoWidget htm css js) = do
        toWidget css
        addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
        toWidget js
        toWidget htm

flatWidget :: PseudoWidget -> Html
flatWidget (PseudoWidget htm css _)
     = (Blaze.style . Blaze.preEscapedText . Txt.toStrict $ renderCss css) <> htm
