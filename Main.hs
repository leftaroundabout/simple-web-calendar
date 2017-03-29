{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveFunctor         #-}


import Yesod

import Data.Text(Text, pack, unpack)
import Data.Maybe
import Data.Ord
import qualified Data.Map as Map
import Data.Traversable
import Data.IORef
import Control.Arrow
import Prelude hiding (mapM)
import Control.Monad hiding (mapM, forM)
import Control.Applicative

import Data.Thyme.Calendar


type Event = Text


data Calendar = Calendar {
    eventsSched :: IORef (Map.Map Day Event)
  }
instance RenderMessage Calendar FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "Calendar" [parseRoutes|
/ HomeR GET POST
/calendar CalendrR GET
|]
instance Yesod Calendar

getHomeR :: Handler Html
getHomeR = do
  Calendar{..} <- getYesod
  
  defaultLayout $ do
   setTitle "Calendar"
   toWidget [lucius| body{background-color: grey;} |]
   [whamlet|
      <h2>Calendar
      <p>...
    |]
  
getCalendrR :: Handler Html
getCalendrR = getHomeR
       
postHomeR :: Handler ()
postHomeR = do
  pwi <- runInputPost $ ireq textField "pwd"
  setSession "pwd" pwi
  redirect CalendrR
   

     


main :: IO ()
main = do
   noDates <- newIORef $ Map.empty
   warp 3735 $ Calendar noDates




