--
-- EPITECH PROJECT, 2023
-- parser
-- File description:
-- parser
--

module Spec where

import Test.HUnit
import Parser (
  parseChar,
  parseAnyChar,
  parseOr,
  parseAnd,
  parseMany,
  parseSome,
  parseUInt,
  parseInt,
  parsePair,
  parseList,
  Parser(..)
  )

assertParse :: (Eq a, Show a) => Parser a -> String -> Maybe (a, String) -> Test
assertParse parser input expected =
  TestCase $ assertEqual ("Parsing: " ++ input) expected (runParser parser input)

testParseChar :: Test
testParseChar =
  TestList
    [ assertParse (parseChar 'a') "abcd" (Just ('a', "bcd")),
      assertParse (parseChar 'z') "abcd" Nothing,
      assertParse (parseChar 'b') "abcd" Nothing,
      assertParse (parseChar 'a') "aaaa" (Just ('a', "aaa"))
    ]

testParseAnyChar :: Test
testParseAnyChar =
  TestList
    [ assertParse (parseAnyChar "bca") "abcd" (Just ('a', "bcd")),
      assertParse (parseAnyChar "xyz") "abcd" Nothing,
      assertParse (parseAnyChar "bca") "cdef" (Just ('c', "def"))
    ]

testParseOr :: Test
testParseOr =
  TestList
    [ assertParse (parseOr (parseChar 'a') (parseChar 'b')) "abcd" (Just ('a', "bcd")),
      assertParse (parseOr (parseChar 'a') (parseChar 'b')) "bcda" (Just ('b', "cda")),
      assertParse (parseOr (parseChar 'a') (parseChar 'b')) "xyz" Nothing
    ]

testParseAnd :: Test
testParseAnd =
  TestList
    [ assertParse (parseAnd (parseChar 'a') (parseChar 'b')) "abcd" (Just (('a', 'b'), "cd")),
      assertParse (parseAnd (parseChar 'a') (parseChar 'b')) "bcda" Nothing,
      assertParse (parseAnd (parseChar 'a') (parseChar 'b')) "acd" Nothing
    ]

testParseMany :: Test
testParseMany =
  TestList
    [ assertParse (parseMany (parseChar ' ')) "foobar" (Just ([], "foobar")),
      assertParse (parseMany (parseChar ' ')) " foobar" (Just (" ", "foobar"))
    ]

testParseSome :: Test
testParseSome =
  TestList
    [ assertParse (parseSome (parseChar ' ')) "foobar" Nothing,
      assertParse (parseSome (parseChar ' ')) " foobar" (Just (" ", "foobar"))
    ]

testParseUInt :: Test
testParseUInt =
  TestList
    [ assertParse (parseUInt) "123" (Just (123, "")),
      assertParse (parseUInt) "0" (Just (0, "")),
      assertParse (parseUInt) "foobar" Nothing
    ]

testParseInt :: Test
testParseInt =
  TestList
    [ assertParse (parseInt) "123" (Just (123, "")),
      assertParse (parseInt) "-42" (Just (-42, "")),
      assertParse (parseInt) "+42" (Just (42, "")),
      assertParse (parseInt) "foobar" Nothing
    ]

testParsePair :: Test
testParsePair =
  TestList
    [ assertParse (parsePair parseInt) "(123 456) foo bar" (Just ((((123, 456)), " foo bar"))),
      assertParse (parsePair parseInt) "(1 2) foo bar" (Just ((((1, 2)), " foo bar"))),
      assertParse (parsePair parseInt) "(123) foo bar" Nothing,
      assertParse (parsePair parseInt) "(foo bar" Nothing
    ]

testParseList :: Test
testParseList =
  TestList
    [ assertParse (parseList parseInt) "(1 2 3 5 7 11 13 17) " (Just ((([1, 2, 3, 5, 7, 11, 13, 17]), " "))),
      assertParse (parseList parseInt) "(1 2 3) foo bar" (Just ((([1, 2, 3]), " foo bar"))),
      assertParse (parseList parseInt) "(foo bar" Nothing,
      assertParse (parseList parseInt) "(1 2 3" Nothing
    ]

main :: IO Counts
main =
  runTestTT $
    TestList
      [ testParseChar,
        testParseAnyChar,
        testParseOr,
        testParseAnd,
        testParseMany,
        testParseSome,
        testParseUInt,
        testParseInt,
        testParsePair,
        testParseList
      ]
