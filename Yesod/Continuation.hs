{-# LANGUAGE MultiParamTypeClasses 
           , ScopedTypeVariables
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , QuasiQuotes
           , TemplateHaskell
           , TypeFamilies
           #-}
module Yesod.Continuation (
    -- Types
    Continuations
  , YesodContinuations(..)
    -- Initialize continuations
  , newContinuations
    -- Maintenance (run from master site's onRequest handler)
  , continuationsOnRequest 
    -- Generic Handlers (work from any subsite)
  , addContinuation
  , continuationRoutes
  ) where

import System.UUID.V4
import Yesod
import Yesod.Handler
import Data.Hashable
import Data.HashMap (HashMap)
import qualified Data.HashMap as H
import Data.DateTime
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Language.Haskell.TH.Syntax

-- Exported Types
data Continuations y = Continuations {
    contTContMap :: TContMap y
  , contTCounter :: TVar Int
  , contPruneInterval :: Int
  }

class (Yesod y, YesodSubRoute (Continuations y) y) => YesodContinuations y where
  yesodContinuations :: y -> Continuations y

-- Internal Types
type SessionKey = String
type ContKey = String

type SessionContMap y = HashMap ContKey (GHandler y y ChooseRep)
type TSessionContMap y = TVar (SessionContMap y)

data ContSession y = ContSession {
    csTSessionContMap :: TSessionContMap y 
  , csTExpire :: TVar DateTime
  }
type TContSession y = TVar (ContSession y)

type ContMap y = HashMap SessionKey (TContSession y)
type TContMap y = TVar (ContMap y)

type ContHandler y a = GHandler (Continuations y) y a

mkYesodSub "Continuations master"
  [ClassP ''Yesod [VarT $ mkName "master"]
  ,ClassP ''YesodContinuations [VarT (mkName "master")]
  ]
  [$parseRoutes|
/#ContKey ContR GET
|]

-- | Initialize Continuations
newContinuations :: Int -> IO (Continuations y)
newContinuations checkInterval = do
  (tcm,tc) <- atomically $ do
    tcm <- newTVar H.empty
    tc <- newTVar 0
    return (tcm,tc)
  return $ Continuations tcm tc checkInterval

-- | init/update expiration date for current session, check if expired sessions should be pruned
-- this should be run during master site's onRequest handler in Yesod class
continuationsOnRequest :: YesodContinuations y => GHandler sub y ()
continuationsOnRequest = runMasterHandler $ contToMasterHandler $ do
  cont <- getContinuations
  let interval = contPruneInterval cont
      tcount = contTCounter cont
  clean <- liftIO $ atomically $ checkCounter interval tcount
  if clean
     then expireContSessions
     else return ()

-- | Exported Generic Site Handlers

-- | Register a continuation
addContinuation :: (YesodContinuations y, HasReps rep) => GHandler y y rep -> GHandler sub y (Route y)
addContinuation hndl = contToSubHandler $ addContinuationI hndl

-- | continuation routes registered in current session
continuationRoutes :: YesodContinuations y => GHandler sub y [Route y]
continuationRoutes = contToSubHandler $ continuationRoutesI


--------------------------------------------------
-- Internal Handlers
--------------------------------------------------
addContinuationI :: (Yesod y, HasReps rep) => GHandler y y rep -> ContHandler y (Route y)
addContinuationI hndl = do
  tscm <- getSessionContMap 
  key <- liftIO $ genUniqueKey tscm $ toChooseRepHandler hndl
  y <- getYesod
  rtm <- getRouteToMaster
  return $ rtm $ ContR key

continuationRoutesI :: Yesod y => ContHandler y [Route y]
continuationRoutesI = do
  rtm <- getRouteToMaster
  tscm <- getSessionContMap 
  keys <- liftIO $ atomically $ do
    scm <- readTVar tscm
    return $ H.keys scm
  return $ map (rtm.ContR) keys

-- | subsite route handler
getContR :: Yesod y => ContKey -> ContHandler y ChooseRep
getContR cid = do
  cont <- popCont cid
  runMasterHandler $ case cont of
       Just hndl -> hndl
       Nothing   -> notFound

getContinuations :: YesodContinuations y => GHandler s y (Continuations y)
getContinuations = yesodContinuations <$> getYesod

contToMasterHandler :: YesodContinuations y => ContHandler y a -> GHandler y y a
contToMasterHandler hndl = do
  y <- getYesod
  let cont = yesodContinuations y
      rtm = fromSubRoute cont y
  toMasterHandlerMaybe rtm yesodContinuations Nothing hndl

contToSubHandler :: YesodContinuations y => ContHandler y a -> GHandler s y a
contToSubHandler = runMasterHandler . contToMasterHandler

notExpired :: DateTime -> TContSession y -> STM Bool
notExpired now s = do
  e <- readTVar . csTExpire =<< readTVar s
  return $ now < e

newContSession :: Int -> IO (TContSession y)
newContSession minutes = do
  tscm <- atomically $ newTVar H.empty
  expire <- addMinutes' minutes <$> getCurrentTime
  tExpire <- atomically $ newTVar expire
  atomically $ newTVar $ ContSession tscm tExpire

-- | update counter, and prune expired sessions if interval has been reached
checkCounter :: Int -> TVar Int -> STM Bool
checkCounter interval tc = do
    v <- readTVar tc
    writeTVar tc $ v + 1 `mod` interval
    return $ v == 0

-- | update session expiration date
contSessionKeepAlive :: YesodContinuations y => ContHandler y ()
contSessionKeepAlive = do
  ka <- clientSessionDuration <$> getYesod
  expire <- addMinutes' ka <$> liftIO getCurrentTime
  texpire <- getContSessionExpire
  liftIO $ atomically $ writeTVar texpire expire

expireContSessions :: YesodContinuations y => ContHandler y ()
expireContSessions = do
  tcm <- getContMap
  liftIO $ do
    t <- getCurrentTime
    deleted <- atomically $ do 
      cm <- readTVar tcm
      let sessions = H.toList cm
      sessions' <- filterM (notExpired t . snd) sessions 
      writeTVar tcm $ H.fromList sessions'
      return $ (length sessions) - (length sessions')
    putStrLn $ "deleted " ++ show deleted ++ " continuation sessions" 

getContMap :: ContHandler y (TContMap y)
getContMap = contTContMap <$> getYesodSub

getContCounter :: YesodContinuations y => ContHandler y (TVar Int)
getContCounter = contTCounter <$> getYesodSub

deleteSession :: YesodContinuations y => SessionKey -> ContHandler y ()
deleteSession k = do
  tcm <- getContMap
  liftIO $ atomically $ do
    cm <- readTVar tcm
    let cm' = H.delete k cm
    writeTVar tcm cm'

getContSessionKey :: ContHandler y (Maybe SessionKey)
getContSessionKey = lookupSession "ContSession"

putContSessionKey :: SessionKey -> ContHandler y ()
putContSessionKey = setSession "ContSession"

getContSessionExpire :: Yesod y => ContHandler y (TVar DateTime)
getContSessionExpire = do
  tcs <- getContSession
  cs <- liftIO $ atomically $ readTVar tcs
  return $ csTExpire cs

getContSession :: Yesod y => ContHandler y (TContSession y)
getContSession = do
  y <- getYesod
  tcm <- getContMap 
  key' <- getContSessionKey 
  let timeout = clientSessionDuration y
  case key' of
       Nothing -> do 
         tcs <- liftIO $ newContSession timeout
         k <- liftIO $ genUniqueKey tcm tcs
         putContSessionKey k
         return tcs
                
       Just k  -> liftIO $ do
         cm <- atomically $ readTVar tcm
         case H.lookup k cm of
              (Just cs) -> return cs
              -- server doesn't have session key, probably because session expired or server restarted
              Nothing     -> do 
                cs <- newContSession timeout
                let cm' = H.insert k cs cm
                atomically $ writeTVar tcm cm'
                return cs

getSessionContMap :: Yesod y => ContHandler y (TSessionContMap y)
getSessionContMap = do
  tcs <- getContSession
  csTSessionContMap <$> (liftIO . atomically $ readTVar tcs)

genUniqueKey :: TVar (HashMap String a) -> a -> IO String
genUniqueKey tm v = do
  k <- show <$> uuid
  b <- atomically $ insertIfNotMember tm k v
  if b 
     then return k
     else genUniqueKey tm v

insertIfNotMember :: (Hashable k, Ord k) => TVar (HashMap k v) -> k -> v -> STM Bool
insertIfNotMember tm k v = do
  m <- readTVar tm
  if H.member k m
     then return False
     else do writeTVar tm $ H.insert k v m
             return True

toChooseRepHandler :: HasReps rep => GHandler s y rep -> GHandler s y ChooseRep
toChooseRepHandler hndl = do
  rep <- hndl
  return $ chooseRep rep

popCont :: Yesod y => ContKey -> ContHandler y (Maybe (GHandler y y ChooseRep))
popCont ckey = do
  scm <- getSessionContMap 
  liftIO $ atomically $ do
    m <- readTVar scm
    let hndl = H.lookup ckey m
        m' = H.delete ckey m
    writeTVar scm m'
    return hndl
