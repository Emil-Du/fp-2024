{-# LANGUAGE InstanceSigs #-}

module Lib2
  ( Query(..),
    Parser,
    parse,
    parseString,
    query,
    BookInfo(..),
    ReaderInfo(..),
    BookGenre(..),
    BookAudience(..),
    State(..),
    emptyState,
    stateTransition,
    parseQuery,
    parseBorrowQuery,
    parseReturnQuery,
    parseAddBookQuery,
    parseAddReaderQuery,
    parseRemoveBookQuery,
    parseRemoveReaderQuery,
    parseMergeQuery,
    parseBookInfo,
    parseReaderInfo,
    parseBookGenre,
    parseBookAudience,
  ) where
    

import Control.Applicative (Alternative (empty), (<|>), many, optional)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT, catchE)
import qualified Control.Monad.State as StateM
import qualified Data.Char as C
import Control.Monad.Trans.Class(lift)


type Parser a = ExceptT String (StateM.State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = StateM.runState (runExceptT parser)

parseSatisfy :: (Char -> Bool) -> Parser Char
parseSatisfy s = do
    input <- StateM.get
    case input of
        [] -> throwE "Unexpected end of input"
        (c:cs) ->
            if s c
                then do
                    StateM.put cs
                    return c
                else throwE $ "LEFT TO CONSUME: " ++ input ++ "\n"

                       

parseChar :: Char -> Parser Char
parseChar c = parseSatisfy (== c)

parseSpace :: Parser Char
parseSpace = parseChar ' '


parseLetter :: Parser Char
parseLetter = parseSatisfy C.isLetter

parseDigit :: Parser Char
parseDigit = parseSatisfy C.isDigit


parseString :: String -> Parser String
parseString [] = return []
parseString (c:cs) = do
    _ <- parseChar c
    rest <- parseString cs
    return (c:rest)

many1 :: Parser a -> Parser [a]
many1 p = do
  first <- p
  rest  <- many p
  return (first : rest) 


data Query
    = BorrowQuery BookInfo ReaderInfo
    | ReturnQuery BookInfo ReaderInfo
    | AddBookQuery BookInfo
    | AddReaderQuery ReaderInfo
    | RemoveBookQuery BookInfo
    | RemoveReaderQuery ReaderInfo
    | MergeQuery BookInfo (Maybe Query)
    deriving (Eq, Show)

data BookInfo = BookInfo Title Author BookGenre BookAudience
    deriving (Eq, Show)

type Title = String
type Author = String

data BookGenre = Fantasy | Detective | Scientific | Dictionary
    deriving (Show, Read, Eq)

data BookAudience = Children | Teenager | Adult
    deriving (Show, Read, Eq)

data ReaderInfo = ReaderInfo Name ReaderID
    deriving (Eq, Show)

type Name = String
type ReaderID = Int

try :: Parser a -> Parser a
try p = do
  input <- lift StateM.get
  result <- catchE p (\err -> lift (StateM.put input) >> throwE err)
  return result

query :: Parser Query
query =
   try parseAddBookQuery
    <|> try parseAddReaderQuery
    <|> try parseReturnQuery
    <|> try parseBorrowQuery
    <|> try parseRemoveBookQuery
    <|> try parseRemoveReaderQuery
    <|> try parseMergeQuery



parseQuery :: String -> Either String Query
parseQuery s =
    case parse query s of
        (Left _, _) -> Left $ "No parser matched"
        (Right q, r) ->
            if null r
                then Right q
                else Left ("Unrecognized characters: " ++ r)

parseBorrowQuery :: Parser Query
parseBorrowQuery = do
    _ <- parseString "borrow "
    book <- parseBookInfo
    _ <- parseSpace
    reader <- parseReaderInfo
    return $ BorrowQuery book reader

parseReturnQuery :: Parser Query
parseReturnQuery = do
    _ <- parseString "return "
    book <- parseBookInfo
    _ <- parseSpace
    reader <- parseReaderInfo
    return $ ReturnQuery book reader

parseAddBookQuery :: Parser Query
parseAddBookQuery = do
    _ <- parseString "add-book "
    book <- parseBookInfo
    return $ AddBookQuery book

parseRemoveBookQuery :: Parser Query
parseRemoveBookQuery = do
    _ <- parseString "remove-book "
    book <- parseBookInfo
    return $ RemoveBookQuery book


parseAddReaderQuery :: Parser Query
parseAddReaderQuery = do
    _ <- parseString "add-reader "
    reader <- parseReaderInfo
    return $ AddReaderQuery reader


parseRemoveReaderQuery :: Parser Query
parseRemoveReaderQuery = do
    _ <- parseString "remove-reader "
    reader <- parseReaderInfo
    return $ RemoveReaderQuery reader

parseMergeQuery :: Parser Query
parseMergeQuery = do
    _ <- parseString "merge "
    book <- parseBookInfo
    rest <- optional $ do
        _ <- parseSpace
        parseMergeQuery
    return $ MergeQuery book rest

parseBookInfo :: Parser BookInfo
parseBookInfo = do
  title <- parseTitle
  _ <- parseSpace
  author <- parseAuthor
  _ <- parseSpace
  genre <- parseBookGenre
  _ <- parseSpace
  audience <- parseBookAudience
  return $ BookInfo title author genre audience

parseTitle :: Parser Title
parseTitle = many1 parseLetter

parseAuthor :: Parser Author
parseAuthor = many1 parseLetter

parseBookGenre :: Parser BookGenre
parseBookGenre = parseData ["Fantasy", "Detective", "Scientific", "Dictionary"]

parseBookAudience :: Parser BookAudience
parseBookAudience = parseData ["Children", "Teenager", "Adult"]

parseData :: (Read a) => [String] -> Parser a
parseData []     = empty
parseData (s:xs) = do
    _ <- parseString s
    return (read s)
    <|> parseData xs

parseReaderInfo :: Parser ReaderInfo
parseReaderInfo = do
  name <- parseName
  _ <- parseSpace
  readerID <- parseReaderID
  return $ ReaderInfo name readerID

parseName :: Parser Name
parseName = many1 parseLetter

parseReaderID :: Parser ReaderID
parseReaderID = read <$> many1 parseDigit


data State = State { books :: [BookInfo], readers :: [ReaderInfo] }

emptyState :: State
emptyState = State [] []

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition s (AddBookQuery book) =
    if bookExists book s
        then Left "Book already exists"
        else Right (Just (show book ++ " added."), addBook book s)

stateTransition s (AddReaderQuery reader) =
    if readerExists reader s
        then Left "Reader already exists"
        else Right (Just (show reader ++ " added."), addReader reader s)

stateTransition s (RemoveBookQuery book) =
    if not (bookExists book s)
        then Left "Book not found."
        else Right (Just (show book ++ " removed."), removeBook book s)

stateTransition s (RemoveReaderQuery reader) =
    if not (readerExists reader s)
        then Left "Reader not found."
        else Right (Just (show reader ++ " removed."), removeReader reader s)

stateTransition s (BorrowQuery book reader) =
    case borrowBook book reader s of
        Left err -> Left err
        Right newState -> Right (Just (show reader ++ " borrowed " ++ show book ++ "."), newState)

stateTransition s (ReturnQuery book reader) =
    case returnBook book reader s of
        Left err -> Left err
        Right newState -> Right (Just (show reader ++ " returned " ++ show book ++ "."), newState)

stateTransition s (MergeQuery book maybeNextQuery) =
    mergeBooks book maybeNextQuery s


bookExists :: BookInfo -> State -> Bool
bookExists book s = book `elem` books s

readerExists :: ReaderInfo -> State -> Bool
readerExists reader s = reader `elem` readers s

addBook :: BookInfo -> State -> State
addBook book s = s { books = book : books s }

addReader :: ReaderInfo -> State -> State
addReader reader s = s { readers = reader : readers s }

removeBook :: BookInfo -> State -> State
removeBook book s = s { books = filter (/= book) (books s) }

removeReader :: ReaderInfo -> State -> State
removeReader reader s = s { readers = filter (/= reader) (readers s) }

borrowBook :: BookInfo -> ReaderInfo -> State -> Either String State
borrowBook book reader s
    | not (bookExists book s) = Left "Book not found"
    | not (readerExists reader s) = Left "Reader not found"
    | otherwise = Right s 

returnBook :: BookInfo -> ReaderInfo -> State -> Either String State
returnBook book reader s
    | not (bookExists book s) = Left "Book not found"
    | not (readerExists reader s) = Left "Reader not found"
    | otherwise = Right s 

mergeBooks :: BookInfo -> Maybe Query -> State -> Either String (Maybe String, State)
mergeBooks book Nothing s = Right (Just "Book merged", addBook book s)
mergeBooks book (Just mergequery) s =
    case stateTransition (addBook book s) mergequery of
        Left err -> Left err
        Right (_, newState) -> Right (Just "Book merged and additional query processed", newState)


