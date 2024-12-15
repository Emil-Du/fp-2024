{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Monad.Free (Free (..), liftF)
import Data.ByteString
import Data.String.Conversions
import Control.Lens
import qualified Lib2
import qualified Lib3
import Network.Wreq
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

interpretatorBatch :: LibraryProgram a -> IO a
interpretatorBatch libProg = interpretBatch libProg []

interpretBatch :: LibraryProgram a -> [Lib2.Query] -> IO a
interpretBatch (Pure a) batch = flushBatch batch >> return a
interpretBatch (Free step) batch = case step of
  AddBook book next -> interpretBatch (next ()) (batch ++ [Lib2.AddBookQuery book]) 
  RemoveBook book next -> interpretBatch (next ()) (batch ++ [Lib2.RemoveBookQuery book]) 
  AddReader reader next -> interpretBatch (next ()) (batch ++ [Lib2.AddReaderQuery reader])
  RemoveReader reader next -> interpretBatch (next ()) (batch ++ [Lib2.RemoveReaderQuery reader]) 
  Borrow book reader next -> interpretBatch (next ()) (batch ++ [Lib2.BorrowQuery book reader])
  Return book reader next -> interpretBatch (next ()) (batch ++ [Lib2.ReturnQuery book reader])
  Merge book query next -> interpretBatch (next ()) (batch ++ [Lib2.MergeQuery book query]) 
  Save next -> flushBatch batch >> postToServer "save" >> interpretBatch (next ()) []  
  Load next -> flushBatch batch >> postToServer "load" >> interpretBatch (next ()) [] 

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


program :: LibraryProgram String
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
  return ("Books and operations executed sucessfully")

main :: IO ()
main = do
  putStrLn "Running with Single Request Interpreter:\n"
  _ <- interpretatorSingle program
 -- putStrLn "\nRunning with Batch Request Interpreter:\n"
 -- _ <- interpretatorBatch program
  return ()