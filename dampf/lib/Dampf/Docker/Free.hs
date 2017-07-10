module Dampf.Docker.Free
  ( -- * Run Docker DSL
    runDockerT
    -- * Docker DSL
  , build
  , rm
  , run
  , stop
  ) where

import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.Free       (liftF, iterT)
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import           System.Process.Typed

import           Dampf.Docker.Types


-- Interpreter

runDockerT :: (MonadIO m) => DockerT m a -> m a
runDockerT = iterT dockerIter


dockerIter :: (MonadIO m) => DockerF (m a) -> m a
dockerIter (Build t i next)   = interpBuild t i >> next
dockerIter (Rm c next)        = interpRm c >>= next
dockerIter (Run c i p e next) = interpRun c i p e >> next
dockerIter (Stop c next)      = interpStop c >> next


interpBuild :: (MonadIO m) => String -> FilePath -> m ()
interpBuild t i = void $ runProcess process
  where
    process = proc "docker" ["build", "-t", t, i]


interpRm :: (MonadIO m) => String -> m String
interpRm c = do
    (_, o, _) <- readProcess process
    return . T.unpack $ T.decodeUtf8 o
  where
    process = proc "docker" ["rm", c]


-- docker run -d --restart=always --net=host --name={platform} -p 3012:3012 \
--      {filocore} {filocore platform}
--
-- each port p -> ["-p", "{p}:{p}"]


interpRun :: (MonadIO m)
    => String -> String -> Maybe [Int] -> Maybe String -> m ()
interpRun c i p e = do
    liftIO $ print process
    void $ runProcess process
  where
    ports   = concatMap (\x -> ["-p", show x ++ ":" ++ show x]) (fromMaybe [] p)
    cmd     = fromMaybe "" e

    process = proc "docker" $ ["run", "-d", "--restart=always", "--net=host"
        , "--name=" ++ c
        ] ++ ports ++ [i] ++ words cmd


interpStop :: (MonadIO m) => String -> m ()
interpStop c = void $ runProcess process
  where
    process = proc "docker" ["stop", c]


-- DSL

build :: (MonadIO m) => String -> FilePath -> DockerT m ()
build t i = liftF (Build t i ())


rm :: (MonadIO m) => String -> DockerT m String
rm c = liftF (Rm c id)


run :: (MonadIO m)
    => String -> String -> Maybe [Int] -> Maybe String -> DockerT m ()
run c i p e = liftF (Run c i p e ())


stop :: (MonadIO m) => String -> DockerT m ()
stop c = liftF (Stop c ())

