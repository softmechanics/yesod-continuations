{-# LANGUAGE MultiParamTypeClasses 
           , ScopedTypeVariables
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           #-}
module Yesod.Continuation where

import System.UUID.V4
import Yesod
import Data.Hashable
import Data.HashMap as H
import Data.DateTime
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad

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

data ContState y = ContState {
    tContMap :: TContMap y
  , tCounter :: TVar Int
  }

class Yesod y => YesodContinuations y where
  getContState :: GHandler s y (ContState y)

  -- | number of requests between pruning of expired sessions
  getContPruneInterval :: GHandler s y Int

  -- | contination route
  getContinuationRoute :: y -> ContKey -> Route y

-- | Initialize ContState
newContState :: IO (ContState y)
newContState = do
  (tcm,tc) <- atomically $ do
    tcm <- newTVar H.empty
    tc <- newTVar 0
    return (tcm,tc)
  return $ ContState tcm tc

-- | init/update expiration date for current session, check if expired sessions should be pruned
-- this should be run during onRequest in Yesod class
checkCont :: YesodContinuations y => GHandler s y ()
checkCont = do
  interval <- getContPruneInterval
  tcount <- getContCounter
  clean <- liftIO $ atomically $ checkCounter interval tcount
  if clean
     then expireContSessions
     else return ()

-- | Register a continuation
addCont :: (YesodContinuations y, HasReps rep) => GHandler y y rep -> GHandler s y (Route y)
addCont hndl = do
  tscm <- getSessionContMap 
  key <- liftIO $ genUniqueKey tscm $ toChooseRepHandler hndl
  y <- getYesod
  return $ getContinuationRoute y key

-- | Run a continuation
contHandler :: YesodContinuations y => ContKey -> GHandler y y ChooseRep
contHandler cid = do
  cont <- popCont cid
  case cont of
       Just hndl -> hndl
       Nothing   -> notFound

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
contSessionKeepAlive :: YesodContinuations y => GHandler s y ()
contSessionKeepAlive = do
  ka <- clientSessionDuration <$> getYesod
  expire <- addMinutes' ka <$> liftIO getCurrentTime
  texpire <- getContSessionExpire
  liftIO $ atomically $ writeTVar texpire expire

expireContSessions :: YesodContinuations y => GHandler s y ()
expireContSessions = do
  tcm <- getContMap
  liftIO $ do
    t <- getCurrentTime
    deleted <- atomically $ do 
      cm <- readTVar tcm
      let sessions = toList cm
      sessions' <- filterM (notExpired t . snd) sessions 
      writeTVar tcm $ fromList sessions'
      return $ (length sessions) - (length sessions')
    putStrLn $ "deleted " ++ show deleted ++ " continuation sessions" 

getContMap :: YesodContinuations y => GHandler s y (TContMap y)
getContMap = tContMap <$> getContState

getContCounter :: YesodContinuations y => GHandler s y (TVar Int)
getContCounter = tCounter <$> getContState

deleteSession :: YesodContinuations y => SessionKey -> GHandler s y ()
deleteSession k = do
  tcm <- getContMap
  liftIO $ atomically $ do
    cm <- readTVar tcm
    let cm' = H.delete k cm
    writeTVar tcm cm'

getContSessionKey :: YesodContinuations y => GHandler s y (Maybe SessionKey)
getContSessionKey = lookupSession "ContSession"

putContSessionKey :: YesodContinuations y => SessionKey -> GHandler s y ()
putContSessionKey = setSession "ContSession"

getContSessionExpire :: YesodContinuations y => GHandler s y (TVar DateTime)
getContSessionExpire = do
  tcs <- getContSession
  cs <- liftIO $ atomically $ readTVar tcs
  return $ csTExpire cs

getContSession :: YesodContinuations y => GHandler s y (TContSession y)
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

getSessionContMap :: YesodContinuations y => GHandler s y (TSessionContMap y)
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
  if member k m
     then return False
     else do writeTVar tm $ H.insert k v m
             return True

toChooseRepHandler :: HasReps rep => GHandler s y rep -> GHandler s y ChooseRep
toChooseRepHandler hndl = do
  rep <- hndl
  return $ chooseRep rep

popCont :: YesodContinuations y => ContKey -> GHandler s y (Maybe (GHandler y y ChooseRep))
popCont ckey = do
  scm <- getSessionContMap 
  liftIO $ atomically $ do
    m <- readTVar scm
    let hndl = H.lookup ckey m
        m' = H.delete ckey m
    writeTVar scm m'
    return hndl

contKeys :: YesodContinuations y => GHandler s y [ContKey]
contKeys = do
  tscm <- getSessionContMap 
  liftIO $ atomically $ do
    scm <- readTVar tscm
    return $ H.keys scm
  

