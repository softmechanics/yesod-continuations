{-# LANGUAGE QuasiQuotes
           , TemplateHaskell
           , MultiParamTypeClasses
           , TypeFamilies
           , FlexibleInstances
           #-}

import Yesod
import Yesod.Continuation 
import Control.Applicative
import Data.List
import Data.DateTime

data Test = Test { tContinuations :: Continuations Test }

type TestContinuations = Continuations Test

mkYesod "Test" [$parseRoutes|
/ RootR GET
/cont ContSubR TestContinuations tContinuations
|]

instance Yesod Test where 
  approot _ = ""

  -- continuations and session cookies and expire in 1 minute
  clientSessionDuration _ = 1

  -- reset expiration date for current session's continuation, 
  -- and prune expired sessions
  onRequest = continuationsOnRequest

instance YesodContinuations Test where
  yesodContinuations = tContinuations 
instance YesodSubRoute (Continuations Test) Test where
  fromSubRoute _ _ = ContSubR

getRootR :: GHandler Test Test RepHtml
getRootR = do
  t <- liftIO $ getCurrentTime
  addContinuation $ dateTimeHtml t
  addContinuation $ dateTimeJson t
  routes <- continuationRoutes
  defaultLayout $ addHamlet [$hamlet|
    $forall routes route
      %a!href=@route@ @route@
      %br
  |]

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
  cont <- newContinuations 1
  basicHandler 3000 $ Test cont

