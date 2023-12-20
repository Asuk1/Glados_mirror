{-
-- EPITECH PROJECT, 2023
-- parser
-- File description:
-- parser
-}

module Parser (
    parseChar,
    parseAnyChar,
    parseOr,
    parseAnd,
    parseSome,
    parseMany,
    parseUInt,
    parseInt,
    parsePair,
    parseList, Parser(..)) where

import Data.Char ()

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

parseChar :: Char -> Parser Char
parseChar a = Parser $ \input ->
  case input of
    (b:bx) | a == b -> Just (a, bx)
    (_:_) -> Nothing
    [] -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser $ const Nothing
parseAnyChar (a:ax) = Parser $ \input ->
  case input of
    (b:bx) | a == b -> Just (a, bx)
    (_:_) -> runParser (parseAnyChar ax) input
    [] -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser $ \s ->
  case runParser p1 s of
    Just (a, s') -> Just (a, s')
    Nothing -> runParser p2 s

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser $ \s ->
  case runParser p1 s of
    Just (a, s') ->
      case runParser p2 s' of
        Just (b, s'') -> Just ((a, b), s'')
        Nothing -> Nothing
    Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser $ \s ->
  case runParser p1 s of
    Just (a, s') ->
      case runParser p2 s' of
        Just (b, s'') -> Just (f a b, s'')
        Nothing -> Nothing
    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \s ->
  case runParser p s of
    Just (a, s') ->
      case runParser (parseMany p) s' of
        Just (as, s'') -> Just (a : as, s'')
        Nothing -> Just ([a], s')
    Nothing -> Just ([], s)

parseSome :: Parser a -> Parser [a]
parseSome p = Parser $ \s ->
  case runParser p s of
    Just (a, s') ->
      case runParser (parseMany p) s' of
        Just (as, s'') -> Just (a : as, s'')
        Nothing -> Nothing
    Nothing -> Nothing

parseUInt :: Parser Int
parseUInt = Parser $ \s ->
  case runParser (parseSome (parseAnyChar ['0' .. '9'])) s of
    Just (as, s') -> Just (read as, s')
    Nothing -> Nothing

parseInt :: Parser Int
parseInt = Parser $ \s ->
  case runParser (parseOr (parseChar '-') (parseChar '+')) s of
    Just (c, s') ->
      case runParser parseUInt s' of
        Just (i, s'') -> Just (if c == '-' then -i else i, s'')
        Nothing -> Nothing
    Nothing -> runParser parseUInt s

parsePair :: Parser a -> Parser (a, a)
parsePair p = Parser $ \s ->
  case runParser (parseAndWith (,) (parseChar '(') (parseAndWith (,) p (parseAndWith (,) (parseChar ' ') p))) s of
    Just ((_, (a, (_, b))), ')' : s') -> Just ((a, b), s')
    _ -> Nothing

parseList :: Parser a -> Parser [a]
parseList p = Parser $ \s ->
  case runParser (parseChar '(') s of
    Just (_, s') -> parseList' p [] s'
    Nothing -> Nothing
  where
    parseList' :: Parser a -> [a] -> String -> Maybe ([a], String)
    parseList' parser acc s' =
      case runParser parser s' of
        Just (x, s'') ->
          case runParser (parseChar ' ') s'' of
            Just (_, s''') -> parseList' parser (acc ++ [x]) (dropWhile (== ' ') s''')
            Nothing ->
              case runParser (parseChar ')') s'' of
                Just (_, s''') -> Just (acc ++ [x], s''')
                Nothing -> Nothing
        Nothing ->
          case runParser (parseChar ')') s' of
            Just (_, s'') -> Just (acc, s'')
            Nothing -> Nothing