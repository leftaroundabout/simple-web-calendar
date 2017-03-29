{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveFunctor         #-}


import Yesod
import Text.Lucius

import System.Environment (getArgs)

import Data.Text(Text)
import qualified Data.Text as Txt
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

import Data.Thyme
import Data.Thyme.Calendar.WeekDate


type User = Text

data Event = Event {
       _eventOrganiser :: User
     , _eventDescription :: Text
     }

data Permission = Permission {
       _permissionViewAll :: Bool
     , _permissionPostAll :: Bool
     }

data Calendar = Calendar {
    eventsSched :: IORef (Map.Map Day Event)
  , userPermissions :: Map.Map User Permission
  }
instance RenderMessage Calendar FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "Calendar" [parseRoutes|
/ HomeR GET
/calendar CalendrR GET POST
/setname SetNameR GET POST
|]
instance Yesod Calendar

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
        Calendar eventsState allPermissions <- getYesod
        knownEvents <- liftIO $ readIORef eventsState
        t <- liftIO getCurrentTime
        let today = t^._utctDay
        
        defaultLayout $ do
         setTitle "Calendar"
         mapM_ toWidget [[lucius|body {background-color: grey;}|], requestFormStyles]
         [whamlet|
            <p>
             #{dispEventCalendr thisUser today knownEvents}
          |]
     Nothing -> do
        setUltDestCurrent
        redirect SetNameR
       
postCalendrR :: Handler ()
postCalendrR = do
  newEvDay <- read . Txt.unpack <$> runInputPost (ireq textField "day")
  newEvent <- runInputPost $ iopt textField "event"
  Calendar eventsState permission <- getYesod
  oldEvent <- liftIO $ Map.lookup newEvDay <$> readIORef eventsState
  thisUser <- determineUser
  case thisUser of
    Just (usr, Permission _ writeAny) -> case (oldEvent,newEvent) of
        (Nothing, Just new) -> liftIO . modifyIORef eventsState
                     . Map.insert newEvDay $ Event usr new
        _ -> return ()
    Nothing -> redirect SetNameR
  redirect CalendrR
   

dispEventCalendr :: (User, Permission) -> Day -> Map.Map Day Event -> Html
dispEventCalendr usr day₀ events = [shamlet|
           <table class=calendar>
             $forall week <- daysTable
               <tr class=week>
                 $forall day <- week
                  <td class=day>
                    #{dispDay usr events day}
         |]
 where daysTable = groupBy ((==)`on`view (mondayWeek . _mwWeek))
                    $ take 511 [day₀ & mondayWeek . _mwDay .~ 1 ..]

requestFormStyles :: t -> Css
requestFormStyles = [lucius|
               form .request-day {font-size: 50%;}
               form .event-enter-button {display: none;} |]

dispDay :: (User, Permission) -> Map.Map Day Event -> Day -> Html
dispDay (usr, Permission viewAll _) events d = case Map.lookup d events of
    Just (Event evUsr ev) -> if viewAll || usr==evUsr
        then [shamlet|
                 <form method=post>
                  <input class=request-day
                         type=text name=day
                         value="#{show d}">
                  <input class=event-request
                         type=text name=event
                         value="#{ev}">
                  <input class=event-enter-button
                         type=submit
                         value=enter>
               |]
        else [shamlet| #{show d} |]
    Nothing -> dispDay (usr, Permission True False) (Map.singleton d $ Event usr "") d
 where dayId = "day" ++ filter isAlphaNum (show d)


determineUser :: Handler (Maybe (User, Permission))
determineUser = do
    sessionUser <- lookupSession "username"
    Calendar _ allPermissions <- getYesod
    return $ case sessionUser of
        Just u -> Just $ case Map.lookup u allPermissions of
                   Just p -> (u, p)
                   Nothing -> (u, Permission False False)
        Nothing -> Nothing


main :: IO ()
main = do
   args <- getArgs
   let permissions = parsePermissions args
   noDates <- newIORef $ Map.empty
   warp 3735 $ Calendar noDates permissions


parsePermissions :: [String] -> Map.Map User Permission
parsePermissions [] = Map.empty
parsePermissions (arg:args)
 | "--superuser="`isPrefixOf`arg  = Map.insert
                                      (Txt.pack . filter (/='"') . tail
                                             $ dropWhile (/='=') arg)
                                      (Permission True True)
                                     $ parsePermissions args
 | otherwise  = parsePermissions args


