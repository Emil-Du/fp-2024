{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lib2
  ( Query(..),
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
    parseBookAudience
  ) where

import qualified Data.Char as C


type Parser a = String -> Either String (a, String)

-- Basic Parsers

parseChar :: Char -> Parser Char
parseChar _ [] = Left "Cannot find character in an empty input"
parseChar c s@(h : t) = if c == h then Right (c, t) else Left ("Expected '" ++ [c] ++ "' but found '" ++ [h] ++ "' in " ++ s)

parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h : t) = if C.isLetter h then Right (h, t) else Left (s ++ " does not start with a letter")

parseSpace :: Parser Char
parseSpace = parseChar ' '

parseDigit :: Parser Char
parseDigit [] = Left "Cannot find any digits in an empty input"
parseDigit s@(h : t) = if C.isDigit h then Right (h, t) else Left (s ++ " does not start with a digit")

parseString :: String -> Parser String
parseString [] s = Right ([], s)  
parseString (c:cs) s = case parseChar c s of
    Left err -> Left err
    Right (_, r1) -> case parseString cs r1 of
        Left err -> Left err
        Right (v2, r2) -> Right (c:v2, r2)

-- Helpers

and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 f p1 p2 s = case p1 s of
  Left err -> Left err
  Right (v1, r1) -> case p2 r1 of
    Left err -> Left err
    Right (v2, r2) -> Right (f v1 v2, r2)

and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 f p1 p2 p3 s = case p1 s of
  Left err -> Left err
  Right (v1, r1) -> case p2 r1 of
    Left err -> Left err
    Right (v2, r2) -> case p3 r2 of
      Left err -> Left err
      Right (v3, r3) -> Right (f v1 v2 v3, r3)

and4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4 f p1 p2 p3 p4 s = case p1 s of
  Left err -> Left err
  Right (v1, r1) -> case p2 r1 of
    Left err -> Left err
    Right (v2, r2) -> case p3 r2 of
      Left err -> Left err
      Right (v3, r3) -> case p4 r3 of
        Left err -> Left err
        Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)

orX :: [Parser a] -> Parser a
orX [] _ = Left "No parser matched"
orX (p : ps) s = case p s of
  Left _ -> orX ps s
  Right res -> Right res

many1 :: Parser a -> Parser [a]
many1 p s = case p s of
  Left err -> Left err
  Right (v1, r1) -> case many1' r1 of
    (v2, r2) -> Right (v1:v2, r2)
  where
    many1' s2 = case p s2 of
      Left _ -> ([], s2)
      Right (v2, r2) -> let (vs, r3) = many1' r2 in (v2 : vs, r3)


-- Data Types

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

parseQuery :: String -> Either String Query
parseQuery s =
    case orX
        [ parseBorrowQuery
        , parseReturnQuery
        , parseAddBookQuery
        , parseAddReaderQuery
        , parseRemoveBookQuery
        , parseRemoveReaderQuery
        , parseMergeQuery
        ] s of
            Left e -> Left e
            Right (q, _) -> Right q

-- <borrow-command> ::= "borrow" <book-info> <reader-info>
parseBorrowQuery :: Parser Query
parseBorrowQuery =
    and4 (\_ b _ r -> BorrowQuery b r) (parseString "borrow ") parseBookInfo parseSpace parseReaderInfo

-- <return-command> ::= "return" <book-info> <reader-info>
parseReturnQuery :: Parser Query
parseReturnQuery =
    and4 (\_ b _ r -> ReturnQuery b r) (parseString "return ") parseBookInfo parseSpace parseReaderInfo

-- <add-book-command> ::= "add-book" <book-info>
parseAddBookQuery :: Parser Query
parseAddBookQuery =
    and2 (\_ b -> AddBookQuery b) (parseString "add-book ") parseBookInfo

-- <add-reader-command> ::= "add-reader" <reader-info>
parseAddReaderQuery :: Parser Query
parseAddReaderQuery =
    and2 (\_ r -> AddReaderQuery r) (parseString "add-reader ") parseReaderInfo

-- <remove-book-command> ::= "remove-book" <book-info>
parseRemoveBookQuery :: Parser Query
parseRemoveBookQuery =
    and2 (\_ b -> RemoveBookQuery b) (parseString "remove-book ") parseBookInfo

-- <remove-reader-command> ::= "remove-reader" <reader-info>
parseRemoveReaderQuery :: Parser Query
parseRemoveReaderQuery =
    and2 (\_ r -> RemoveReaderQuery r) (parseString "remove-reader ") parseReaderInfo


-- <merge-command> ::= "merge" <book-info> <merge-command> | "merge" <book-info>
parseMergeQuery :: Parser Query
parseMergeQuery = orX [and4 (\_ bi _ mq -> MergeQuery bi (Just mq)) (parseString "merge ") parseBookInfo parseSpace parseMergeQuery
                       ,and2 (\_ bi -> MergeQuery bi Nothing) (parseString "merge ") parseBookInfo
                      ]


-- <book-info> ::= <title> <author> <book-genre> <book-audience>
parseBookInfo :: Parser BookInfo
parseBookInfo input = 
  case parseTitle input of
    Left err -> Left err
    Right (title, rest1) -> 
      case parseSpace rest1 of
        Left err -> Left err
        Right (_, rest2) -> 
          case parseAuthor rest2 of
            Left err -> Left err
            Right (author, rest3) -> 
              case parseSpace rest3 of
                Left err -> Left err
                Right (_, rest4) -> 
                  case parseBookGenre rest4 of
                    Left err -> Left err
                    Right (genre, rest5) -> 
                      case parseSpace rest5 of
                        Left err -> Left err
                        Right (_, rest6) -> 
                          case parseBookAudience rest6 of
                            Left err -> Left err
                            Right (audience, rest7) -> 
                              Right (BookInfo title author genre audience, rest7)

-- <title> ::= <string>
parseTitle :: Parser Title
parseTitle = many1 parseLetter

-- <author> ::= <string>
parseAuthor :: Parser Author
parseAuthor = many1 parseLetter

-- <book-genre> ::= "fantasy" | "detective" | "scientific" | "dictionary"
parseBookGenre :: Parser BookGenre
parseBookGenre s =
    case parseData [Fantasy, Detective, Scientific, Dictionary] s of
        Left e -> Left ("Could not parse Book Genre: " ++ e)
        Right (g, r) -> Right (read g, r)

-- <book-audience> ::= "children" | "teenager" | "adult"
parseBookAudience :: Parser BookAudience
parseBookAudience s =
    case parseData [Children, Teenager, Adult] s of
        Left e -> Left ("Could not parse Book Audience: " ++ e)
        Right (a, r) -> Right (read a, r)

parseData :: (Show a) => [a] -> Parser String
parseData options = orX (map parseString (map show options))

-- <reader-info> ::= <name> <reader-id>
parseReaderInfo :: Parser ReaderInfo
parseReaderInfo = and3 (\n _ r -> ReaderInfo n r) parseName parseSpace parseReaderID

-- <name> ::= <string>
parseName :: Parser Name
parseName = many1 parseLetter

-- <reader-id> ::= <int>
parseReaderID :: Parser ReaderID
parseReaderID input =
  case many1 parseDigit input of
    Left err -> Left err
    Right (digits, rest) -> Right (read digits, rest)


-- States and State Transitions


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
mergeBooks book (Just query) s =
    case stateTransition (addBook book s) query of
        Left err -> Left err
        Right (_, newState) -> Right (Just "Book merged and additional query processed", newState)

