{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Lens
import Control.Monad.Free (Free (..), liftF)
import Data.ByteString
import Data.String.Conversions
import qualified Lib2
import qualified Lib3
import Network.Wreq

-- BNF: root = "add-book " book | "remove-book " book | "add-reader " reader | "remove-reader " reader | "borrow " book reader | "return " book reader | "merge " book
data MyDomainAlgebra a
  = AddBook Lib2.BookInfo a
  | RemoveBook Lib2.BookInfo a
  | AddReader Lib2.ReaderInfo a
  | RemoveReader Lib2.ReaderInfo a
  | Borrow Lib2.BookInfo Lib2.ReaderInfo a
  | Return Lib2.BookInfo Lib2.ReaderInfo a
  | Merge Lib2.BookInfo (Maybe Lib2.Query) a
  | Save a
  | Load a
  deriving (Functor)

type LibraryProgram = Free MyDomainAlgebra

addBook :: Lib2.BookInfo -> LibraryProgram ()
addBook book = liftF $ AddBook book ()

removeBook :: Lib2.BookInfo -> LibraryProgram ()
removeBook book = liftF $ RemoveBook book ()

addReader :: Lib2.ReaderInfo -> LibraryProgram ()
addReader reader = liftF $ AddReader reader ()

removeReader :: Lib2.ReaderInfo -> LibraryProgram ()
removeReader reader = liftF $ RemoveReader reader ()

borrowBook :: Lib2.BookInfo -> Lib2.ReaderInfo -> LibraryProgram ()
borrowBook book reader = liftF $ Borrow book reader ()

returnBook :: Lib2.BookInfo -> Lib2.ReaderInfo -> LibraryProgram ()
returnBook book reader = liftF $ Return book reader ()

mergeBooks :: Lib2.BookInfo -> Maybe Lib2.Query -> LibraryProgram ()
mergeBooks book query = liftF $ Merge book query ()


interpretSingle :: LibraryProgram a -> IO a
interpretSingle (Pure a) = return a
interpretSingle (Free step) = do
  next <- runStepSingle step
  interpretSingle next
  where
    runStepSingle :: MyDomainAlgebra a -> IO a
    runStepSingle (AddBook book next) = sendSingleStatement (Lib2.AddBookQuery book) >> return next
    runStepSingle (RemoveBook book next) = sendSingleStatement (Lib2.RemoveBookQuery book) >> return next
    runStepSingle (AddReader reader next) = sendSingleStatement (Lib2.AddReaderQuery reader) >> return next
    runStepSingle (RemoveReader reader next) = sendSingleStatement (Lib2.RemoveReaderQuery reader) >> return next
    runStepSingle (Borrow book reader next) = sendSingleStatement (Lib2.BorrowQuery book reader) >> return next
    runStepSingle (Return book reader next) = sendSingleStatement (Lib2.ReturnQuery book reader) >> return next
    runStepSingle (Merge book query next) = sendSingleStatement (Lib2.MergeQuery book query) >> return next

sendSingleStatement :: Lib2.Query -> IO String
sendSingleStatement = postAsString . Lib3.renderStatements . Lib3.Single

postAsString :: String -> IO String
postAsString s = do
  let rawRequest = cs s :: ByteString
  putStrLn $ "Sending request:\n" ++ cs rawRequest
  resp <- post "http://localhost:3000" rawRequest
  return $ cs $ resp ^. responseBody

interpretBatch :: LibraryProgram a -> IO a
interpretBatch prog = interpretBatch' prog []

interpretBatch' :: LibraryProgram a -> [Lib2.Query] -> IO a
interpretBatch' (Pure a) batch = dumpBatch batch >> return a
interpretBatch' (Free step) batch = do
  case step of
    AddBook book next -> interpretBatch' next (batch ++ [Lib2.AddBookQuery book])
    RemoveBook book next -> interpretBatch' next (batch ++ [Lib2.RemoveBookQuery book])
    AddReader reader next -> interpretBatch' next (batch ++ [Lib2.AddReaderQuery reader])
    RemoveReader reader next -> interpretBatch' next (batch ++ [Lib2.RemoveReaderQuery reader])
    Borrow book reader next -> interpretBatch' next (batch ++ [Lib2.BorrowQuery book reader])
    Return book reader next -> interpretBatch' next (batch ++ [Lib2.ReturnQuery book reader])
    Merge book query next -> interpretBatch' next (batch ++ [Lib2.MergeQuery book query])

dumpBatch :: [Lib2.Query] -> IO (Maybe String)
dumpBatch [] = return Nothing
dumpBatch batch = Just <$> sendAsBatch batch

sendAsBatch :: [Lib2.Query] -> IO String
sendAsBatch batch = postAsString (Lib3.renderStatements (Lib3.Batch batch))

type InMemoryState = [String] 

interpretInMemory :: LibraryProgram a -> InMemoryState -> IO (a, InMemoryState)
interpretInMemory (Pure a) state = return (a, state)
interpretInMemory (Free step) state = do
  case step of
    AddBook book next -> interpretInMemory next (state ++ [show (Lib2.AddBookQuery book)])
    RemoveBook book next -> interpretInMemory next (state ++ [show (Lib2.RemoveBookQuery book)])
    AddReader reader next -> interpretInMemory next (state ++ [show (Lib2.AddReaderQuery reader)])
    RemoveReader reader next -> interpretInMemory next (state ++ [show (Lib2.RemoveReaderQuery reader)])
    Borrow book reader next -> interpretInMemory next (state ++ [show (Lib2.BorrowQuery book reader)])
    Return book reader next -> interpretInMemory next (state ++ [show (Lib2.ReturnQuery book reader)])
    Merge book query next -> interpretInMemory next (state ++ [show (Lib2.MergeQuery book query)])


program :: LibraryProgram (String, String)
program = do
  let book1 = Lib2.BookInfo "BookOne" "AuthorOne" Lib2.Fantasy Lib2.Children
      book2 = Lib2.BookInfo "BookTwo" "AuthorTwo" Lib2.Detective Lib2.Adult
      reader1 = Lib2.ReaderInfo "ReaderOne" 101
  addBook book1
  addReader reader1
  borrowBook book1 reader1
  returnBook book1 reader1
  removeReader reader1
  removeBook book1
  addBook book2
  removeBook book2
  mergeBooks book1 (Just (Lib2.MergeQuery book2 Nothing))
  removeBook book1
  removeBook book2
  return ("Books and operations executed", "Success")



main :: IO ()
main = do
  putStrLn "Running with Single Request Interpreter:"
  str <- interpretSingle program
  print str
  putStrLn "\nRunning with Batch Request Interpreter:"
  _ <- interpretBatch program
  putStrLn "\nRunning with In-Memory Interpreter for Testing:"
  (_, state) <- interpretInMemory program []
  putStrLn $ "In-memory state: " ++ show state
  return ()
