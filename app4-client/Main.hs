{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Monad.Free (Free (..), liftF)
import Data.ByteString
import Data.String.Conversions
import Control.Lens
import qualified Lib2
import qualified Lib3
import Network.Wreq
import Control.Concurrent (Chan, newChan, forkIO)
import Control.Concurrent.STM (TVar, newTVarIO)


data MyDomainAlgebra next
  = AddBook Lib2.BookInfo (() -> next)
  | RemoveBook Lib2.BookInfo (() -> next)
  | AddReader Lib2.ReaderInfo (() -> next)
  | RemoveReader Lib2.ReaderInfo (() -> next)
  | Borrow Lib2.BookInfo Lib2.ReaderInfo (() -> next)
  | Return Lib2.BookInfo Lib2.ReaderInfo (() -> next)
  | Merge Lib2.BookInfo (Maybe Lib2.Query) (() -> next)
  | Save (() -> next)
  | Load (() -> next)
  deriving Functor

type LibraryProgram = Free MyDomainAlgebra

addBook :: Lib2.BookInfo -> LibraryProgram ()
addBook book = liftF $ AddBook book id

removeBook :: Lib2.BookInfo -> LibraryProgram ()
removeBook book = liftF $ RemoveBook book id

addReader :: Lib2.ReaderInfo -> LibraryProgram ()
addReader reader = liftF $ AddReader reader id

removeReader :: Lib2.ReaderInfo -> LibraryProgram ()
removeReader reader = liftF $ RemoveReader reader id

borrowBook :: Lib2.BookInfo -> Lib2.ReaderInfo -> LibraryProgram ()
borrowBook book reader = liftF $ Borrow book reader id

returnBook :: Lib2.BookInfo -> Lib2.ReaderInfo -> LibraryProgram ()
returnBook book reader = liftF $ Return book reader id

mergeBooks :: Lib2.BookInfo -> Maybe Lib2.Query -> LibraryProgram ()
mergeBooks book query = liftF $ Merge book query id

saveLibrary :: LibraryProgram ()
saveLibrary = liftF $ Save id

loadLibrary :: LibraryProgram ()
loadLibrary = liftF $ Load id

interpretatorSingle :: LibraryProgram a -> IO a
interpretatorSingle (Pure a) = return a
interpretatorSingle (Free step) = do
  next <- runStepSingle step
  interpretatorSingle next

runStepSingle :: MyDomainAlgebra a -> IO a
runStepSingle (AddBook book next) = sendSingle (Lib2.AddBookQuery book) >> return (next ())
runStepSingle (RemoveBook book next) = sendSingle (Lib2.RemoveBookQuery book) >> return (next ())
runStepSingle (AddReader reader next) = sendSingle (Lib2.AddReaderQuery reader) >> return (next ())
runStepSingle (RemoveReader reader next) = sendSingle (Lib2.RemoveReaderQuery reader) >> return (next ())
runStepSingle (Borrow book reader next) = sendSingle (Lib2.BorrowQuery book reader) >> return (next ())
runStepSingle (Return book reader next) = sendSingle (Lib2.ReturnQuery book reader) >> return (next ())
runStepSingle (Merge book query next) = sendSingle (Lib2.MergeQuery book query) >> return (next ())
runStepSingle (Save next) = postToServer "save" >> return (next ())
runStepSingle (Load next) = postToServer "load" >> return (next ())

sendSingle :: Lib2.Query -> IO String
sendSingle = postToServer . Lib3.renderStatements . Lib3.Single

interpretatorBatch :: LibraryProgram a -> [Lib2.Query] -> IO a
interpretatorBatch (Pure a) batch = flushBatch batch >> return a
interpretatorBatch (Free step) batch = case step of
  AddBook book next -> interpretatorBatch (next ()) (batch ++ [Lib2.AddBookQuery book]) 
  RemoveBook book next -> interpretatorBatch (next ()) (batch ++ [Lib2.RemoveBookQuery book]) 
  AddReader reader next -> interpretatorBatch (next ()) (batch ++ [Lib2.AddReaderQuery reader])
  RemoveReader reader next -> interpretatorBatch (next ()) (batch ++ [Lib2.RemoveReaderQuery reader]) 
  Borrow book reader next -> interpretatorBatch (next ()) (batch ++ [Lib2.BorrowQuery book reader])
  Return book reader next -> interpretatorBatch (next ()) (batch ++ [Lib2.ReturnQuery book reader])
  Merge book query next -> interpretatorBatch (next ()) (batch ++ [Lib2.MergeQuery book query]) 
  Save next -> flushBatch batch >> postToServer "save" >> interpretatorBatch (next ()) []  
  Load next -> flushBatch batch >> postToServer "load" >> interpretatorBatch (next ()) [] 

flushBatch :: [Lib2.Query] -> IO (Maybe String)
flushBatch [] = return Nothing
flushBatch batch = Just <$> sendBatch batch

sendBatch :: [Lib2.Query] -> IO String
sendBatch batch = postToServer (Lib3.renderStatements (Lib3.Batch batch))

postToServer :: String -> IO String
postToServer s = do
  let rawRequest = cs s :: ByteString
  putStrLn $ "Sending request:\n" ++ cs rawRequest
  resp <- post "http://localhost:3000" rawRequest
  return $ cs $ resp ^. responseBody


interpretatorTest :: LibraryProgram a -> IO a
interpretatorTest p = do
  state <- newTVarIO Lib2.emptyState
  chan <- newChan
  _ <- forkIO $ Lib3.storageOpLoop chan
  runTestInterpretator state chan p

runTestInterpretator :: TVar Lib2.State -> Chan Lib3.StorageOp -> LibraryProgram a -> IO a
runTestInterpretator _ _ (Pure a) = return a
runTestInterpretator state chan (Free step) = do
      next <- runStep state chan step
      runTestInterpretator state chan next
   
runStep :: TVar Lib2.State -> Chan Lib3.StorageOp -> MyDomainAlgebra a -> IO a
runStep state chan (AddBook book next) =
  printState state (Lib3.StatementCommand $ Lib3.Single $ Lib2.AddBookQuery book) chan >>
  return (next ())
runStep state chan (RemoveBook book next) =
  printState state (Lib3.StatementCommand $ Lib3.Single $ Lib2.RemoveBookQuery book) chan >>
  return (next ())
runStep state chan (AddReader reader next) =
  printState state (Lib3.StatementCommand $ Lib3.Single $ Lib2.AddReaderQuery reader) chan >>
  return (next ())
runStep state chan (RemoveReader reader next) =
  printState state (Lib3.StatementCommand $ Lib3.Single $ Lib2.RemoveReaderQuery reader) chan >>
  return (next ())
runStep state chan (Borrow book reader next) =
  printState state (Lib3.StatementCommand $ Lib3.Single $ Lib2.BorrowQuery book reader) chan >>
  return (next ())
runStep state chan (Return book reader next) =
  printState state (Lib3.StatementCommand $ Lib3.Single $ Lib2.ReturnQuery book reader) chan >>
  return (next ())
runStep state chan (Merge book query next) =
  printState state (Lib3.StatementCommand $ Lib3.Single $ Lib2.MergeQuery book query) chan >>
  return (next ())
runStep state chan (Save next) =
  printState state Lib3.SaveCommand chan >>
  return (next ())
runStep state chan (Load next) =
  printState state Lib3.LoadCommand chan >>
  return (next ())

printState :: TVar Lib2.State -> Lib3.Command -> Chan Lib3.StorageOp -> IO String
printState state command chan = do
  putStrLn $ "Received command:\n" ++ show command
  res <- Lib3.stateTransition state command chan
  let str = case res of
        Left err -> err
        Right (Just s) -> s
        Right Nothing -> "Done."
  putStrLn "Result:"
  putStrLn str
  return str

program :: LibraryProgram ()
program = do
  let book1 = Lib2.BookInfo "BookOne" "AuthorOne" Lib2.Fantasy Lib2.Children
      book2 = Lib2.BookInfo "BookTwo" "AuthorTwo" Lib2.Detective Lib2.Adult
      reader1 = Lib2.ReaderInfo "ReaderOne" 101
  addBook book1
  addReader reader1
  borrowBook book1 reader1
  returnBook book1 reader1
  saveLibrary
  loadLibrary
  removeReader reader1
  removeBook book1
  addBook book2
  removeBook book2
  mergeBooks book1 (Just (Lib2.MergeQuery book2 Nothing))
  removeBook book1
  removeBook book2

main :: IO ()
main = do
  --putStrLn "Running with single interpreter:\n"
  --_ <- interpretatorSingle program
  --putStrLn "\nRunning with batch interpreter:\n"
  --_ <- interpretatorBatch program []
  --putStrLn "\nRunning with in-memory interpreter:\n"
  _ <- interpretatorTest program
  return ()