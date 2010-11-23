{-# LANGUAGE QuasiQuotes
           , TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           #-}

import Yesod
import Yesod.Continuation
import Control.Applicative
import Data.List
import Data.DateTime

data Test = Test
  { 
    -- easiest place to put this for now 
    tContState :: ContState Test 
  }

mkYesod "Test" [$parseRoutes|
/ RootR GET
/#ContKey ContR GET
|]

instance Yesod Test where 
  approot _ = ""

  -- continuations and session cookies and expire in 1 minute
  clientSessionDuration _ = 1

  -- reset expiration date for current session's continuation, 
  -- and prune expired sessions
  onRequest = checkCont

instance YesodContinuations Test where
  getContState = tContState <$> getYesod

  -- clean up expired continuations every 1 request 
  getContPruneInterval = return 1

  getContinuationRoute _ = ContR

getRootR :: GHandler Test Test RepHtml
getRootR = do
  t <- liftIO $ getCurrentTime
  addCont $ dateTimeHtml t
  addCont $ dateTimeJson t
  keys <- sort <$> contKeys
  defaultLayout $ addHamlet [$hamlet|
    $forall keys key
      %a!href=@ContR key@ $key$
      %br
  |]

getContR :: ContKey -> GHandler Test Test ChooseRep
getContR = contHandler 

dateTimeHtml :: DateTime -> GHandler Test Test RepHtml
dateTimeHtml gent = defaultLayout $ do
  runt <- liftIO getCurrentTime
  addHamlet [$hamlet|
    %h1 HTML Continuation
    %ul
      %li Generated at: $show gent$
      %li Ran at: $show runt$
    |]
      
dateTimeJson :: DateTime -> GHandler Test Test RepJson
dateTimeJson gent = do
  runt <- liftIO getCurrentTime
  jsonToRepJson $ jsonMap [
      ("generated", jsonScalar $ show gent)
    , ("ran", jsonScalar $ show runt)
    ]

main :: IO ()
main = do
  cont <- newContState
  basicHandler 3000 $ Test cont

