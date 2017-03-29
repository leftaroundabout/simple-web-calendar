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

import Data.Text(Text)
import qualified Data.Text as Txt
import Data.Maybe
import Data.List (groupBy)
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


type Event = Text


data Calendar = Calendar {
    eventsSched :: IORef (Map.Map Day Event)
  }
instance RenderMessage Calendar FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "Calendar" [parseRoutes|
/ HomeR GET
/calendar CalendrR GET POST
|]
instance Yesod Calendar

getHomeR :: Handler Html
getHomeR = do
  Calendar eventsState <- getYesod
  knownEvents <- liftIO $ readIORef eventsState
  t <- liftIO getCurrentTime
  let today = t^._utctDay
  
  defaultLayout $ do
   setTitle "Calendar"
   mapM_ toWidget [[lucius|body {background-color: grey;}|], requestFormStyles]
   [whamlet|
      <p>
       #{dispEventCalendr today knownEvents}
    |]
  
getCalendrR :: Handler Html
getCalendrR = getHomeR
       
postCalendrR :: Handler ()
postCalendrR = do
  newEvDay <- read . Txt.unpack <$> runInputPost (ireq textField "day")
  newEvent <- runInputPost $ iopt textField "event"
  Calendar eventsState <- getYesod
  oldEvent <- liftIO $ Map.lookup newEvDay <$> readIORef eventsState
  case (oldEvent,newEvent) of
     (Nothing, Just new) -> liftIO . modifyIORef eventsState $ Map.insert newEvDay new
     _ -> return ()
  redirect CalendrR
   

dispEventCalendr :: Day -> Map.Map Day Event -> Html
dispEventCalendr day₀ events = [shamlet|
           <table class=calendar>
             $forall week <- daysTable
               <tr class=week>
                 $forall day <- week
                  <td class=day>
                    #{dispDay events day}
         |]
 where daysTable = groupBy ((==)`on`view (mondayWeek . _mwWeek))
                    $ take 511 [day₀ & mondayWeek . _mwDay .~ 1 ..]

requestFormStyles :: t -> Css
requestFormStyles = [lucius|
               form .request-day {font-size: 50%;}
               form .event-enter-button {display: none;} |]

dispDay :: Map.Map Day Event -> Day -> Html
dispDay events d = case Map.lookup d events of
    Just ev -> [shamlet| #{ev} |] 
    Nothing -> [shamlet|
                 <form method=post>
                  <input class=request-day
                         type=text name=day
                         value="#{show d}">
                  <input class=event-request
                         type=text name=event>
                  <input class=event-enter-button
                         type=submit
                         value=enter>
               |]
 where dayId = "day" ++ filter isAlphaNum (show d)


main :: IO ()
main = do
   noDates <- newIORef $ Map.empty
   warp 3735 $ Calendar noDates




