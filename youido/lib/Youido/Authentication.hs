{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections, TypeFamilies,
             DeriveGeneric, ExtendedDefaultRules, FlexibleContexts#-}

module Youido.Authentication where

import Youido.Types
import Web.Scotty
import Web.Scotty.Cookie
import Control.Concurrent.STM
import qualified Data.IntMap
import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)
import System.Random
import Text.Read (readMaybe)
import Data.Proxy
import Data.Dynamic
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------
--- TYPES
--------------------------------------------------------------------------
  
class Authenticate u where
  type AuthRoute u
  notPresent :: Proxy u -> Maybe (AuthRoute u)
  
data Auth u a = Auth u a
{-
instance (FromRequest a, Authenticate u) => FromRequest (Auth u a) where
  fromRequest rqpars = case fromRequest rqpars of
    (Just x, _) -> return $ Left x
    (Nothing, Just y) -> return $ Right y
-}


--------------------------------------------------------------------------
--- SERVING
--------------------------------------------------------------------------

insertSessionValue :: TVar (Data.IntMap.IntMap a) -> a -> ActionM ()
insertSessionValue sessions kv = do
      msess <- lookupSession sessions
      case msess of
        Nothing -> if null users then go (Map.empty) else redirect "/login"
        Just (i,u) -> go u




newSession :: TVar (Data.IntMap.IntMap a) -> a -> ActionM ()
newSession tv email = do
  n <- liftIO $ randomRIO (0,99999999999)
  liftIO $ atomically $ modifyTVar' tv (Data.IntMap.insert n email)
  setSimpleCookie "youisess" (pack $ show n)
  return ()

lookupSession :: TVar (Data.IntMap.IntMap a) -> ActionM (Maybe (Int, a))
lookupSession tv = do
  mi <- (>>=readMaybe) . fmap unpack <$> getCookie "youisess"
  case mi of
    Nothing -> return Nothing
    Just i -> do
      mp <- liftIO $ readTVarIO tv
      return $ fmap (i,) $ Data.IntMap.lookup i mp



deleteSession :: TVar (Data.IntMap.IntMap a) -> Int -> ActionM ()
deleteSession tv n = do
  liftIO $ atomically $ modifyTVar' tv (Data.IntMap.delete n)
  deleteCookie "youisess"
  return ()

