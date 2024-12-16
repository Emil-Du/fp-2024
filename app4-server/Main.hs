{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import qualified Lib2
import qualified Lib3
import GHC.Conc (forkIO)
import Web.Scotty (body, post, scotty, ScottyM)

main :: IO ()
main = do
    state <- newTVarIO Lib2.emptyState
    storageChan <- newChan
    _ <- forkIO $ Lib3.storageOpLoop storageChan
    scotty 3000 $ app state storageChan

app :: TVar Lib2.State -> Chan Lib3.StorageOp -> ScottyM ()
app state storageChan = do
    post "/" $ do
        req <- body
        liftIO $ putStrLn $ "Received request: " ++ cs req
        _ <- liftIO $ handleRequest state storageChan (cs req)
        return ()

handleRequest :: TVar Lib2.State -> Chan Lib3.StorageOp -> String -> IO String
handleRequest state storageChan input = 
    case Lib3.parseCommand input of
        Left err -> do
            putStrLn $ "Error parsing request: " ++ err
            return $ "Error: " ++ err
        Right (command, _) -> do
            result <- Lib3.stateTransition state command storageChan
            case result of
                Left err -> do
                    putStrLn $ "Error processing command: " ++ err
                    return $ "Error: " ++ err
                Right output -> do
                    let response = case output of
                            Just s -> s
                            Nothing -> "No response"
                    putStrLn $ "Response: " ++ response
                    return response
