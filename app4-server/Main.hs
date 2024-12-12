{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.State.Strict
  ( MonadIO (liftIO),
  )
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import GHC.Conc (forkIO)
import qualified Lib2
import qualified Lib3
import qualified Web.Scotty as Scty

main :: IO ()
main = do
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  state <- newTVarIO Lib2.emptyState
  _ <- forkIO $ Lib3.storageOpLoop chan
  Scty.scotty 3000 $
    Scty.post "/" $ do
      b <- Scty.body
      liftIO $ putStrLn ("Request was: " ++ cs b)
      response <- liftIO $ process state chan $ cs b
      Scty.text $ cs response

process :: TVar Lib2.State -> Chan Lib3.StorageOp -> String -> IO String
process state storageChan input = case Lib3.parseCommand input of
  Left e -> return e
  Right (cmd, "") -> do
    info <- Lib3.stateTransition state cmd storageChan
    case info of
      Left e -> return e
      Right mb -> return $ fromMaybe "Success" mb
  Right (_, str) -> return $ "Could not parse: " ++ str