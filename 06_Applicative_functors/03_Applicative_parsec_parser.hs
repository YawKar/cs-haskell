module Demo () where

import Control.Applicative ((*>), (<*))
import Data.Char (isSpace)
import Text.Parsec
  ( ParseError,
    Parsec,
    char,
    digit,
    endBy1,
    letter,
    many,
    oneOf,
    parse,
    parseTest,
    satisfy,
    sepBy1,
    sepEndBy1,
    spaces,
    string,
  )
import Text.Parsec.Combinator (count, many1, option)

{-
Right '1'
-}
exampleParseDigit :: Either ParseError Char
exampleParseDigit = parse digit "test simple digit" "12AB"

{-
Left "test simple digit error" (line 1, column 1):
unexpected "A"
expecting digit
-}
exampleParseDigitError :: Either ParseError Char
exampleParseDigitError = parse digit "test simple digit error" "AB12"

{-
Right 'A'
-}
exampleParseLetter :: Either ParseError Char
exampleParseLetter = parse letter "test simple letter" "AB12"

{-
Left "test simple letter error" (line 1, column 1):
unexpected "1"
expecting letter
-}
exampleParseLetterError :: Either ParseError Char
exampleParseLetterError = parse letter "test simple letter error" "12AB"

vowel :: Parsec [Char] u Char
vowel = oneOf "aeiou"

{-
Right 'a'
-}
exampleParseVowel :: Either ParseError Char
exampleParseVowel = parse vowel "test parse vowel 'a'" "a"

{-
Left "test parse vowel 'a'" (line 1, column 1):
unexpected "b"
-}
exampleParseVowelError :: Either ParseError Char
exampleParseVowelError = parse vowel "test parse vowel 'a'" "bcd"

{-
What do type parameters of the Parsec type constructor mean?

Parsec [Char] u Char
[Char]: type of input stream
u: user's stateful extension (will be covered later)
Char: type of output value
-}

-- many1 applies given parser 1 or more times and returns list of parsed elements
-- Right "123"
exampleParse1OrMoreDigits :: Either ParseError [Char]
exampleParse1OrMoreDigits = parse (many1 digit) "test parse 1 or more digits" "123"

{-
Left "test parse 1 or more digits" (line 1, column 1):
unexpected "a"
expecting digit
-}
exampleParse1OrMoreDigitsError :: Either ParseError [Char]
exampleParse1OrMoreDigitsError = parse (many1 digit) "test parse 1 or more digits" "abc123"

-- many applies given parser 0 or more times and returns list of parsed elements
-- Right ""
exampleParse0OrMoreDigits :: Either ParseError [Char]
exampleParse0OrMoreDigits = parse (many digit) "test parse 1 or more digits" "abc"

-- `count` applies given parser specified number of times
-- Right "123"
exampleParse3Digits :: Either ParseError [Char]
exampleParse3Digits = parse (count 3 digit) "test parse 3 digits" "1234abc"

{-
Left "test parse 3 digits" (line 1, column 3):
unexpected "a"
expecting digit
-}
exampleParse3DigitsError :: Either ParseError [Char]
exampleParse3DigitsError = parse (count 3 digit) "test parse 3 digits" "12abc"

-- Right ["123"]
exampleParseDPostfixed3DigitNumber1 :: Either ParseError [String]
exampleParseDPostfixed3DigitNumber1 = parse (count 3 digit `endBy1` char 'd') "test parse 123d number" "123d"

-- Right ["123","456"]
exampleParseDPostfixed3DigitNumber2 :: Either ParseError [String]
exampleParseDPostfixed3DigitNumber2 = parse (count 3 digit `endBy1` char 'd') "test parse 123d number" "123d456d"

{-
Left "test parse 123d number" (line 1, column 4):
unexpected "b"
expecting "d"
-}
exampleParseDPostfixed3DigitNumberError1 :: Either ParseError [String]
exampleParseDPostfixed3DigitNumberError1 = parse (count 3 digit `endBy1` char 'd') "test parse 123d number" "123b"

{-
Left "test parse 123d number" (line 1, column 3):
unexpected "b"
expecting digit
-}
exampleParseDPostfixed3DigitNumberError2 :: Either ParseError [String]
exampleParseDPostfixed3DigitNumberError2 = parse (count 3 digit `endBy1` char 'd') "test parse 123d number" "12b433b"

{-
Left "test parse 123d number" (line 1, column 11):
unexpected end of input
expecting digit
-}
exampleParseDPostfixed3DigitNumberError3 :: Either ParseError [String]
exampleParseDPostfixed3DigitNumberError3 = parse (count 3 digit `endBy1` char 'd') "test parse 123d number" "123d456d78"

getList :: Parsec String u [String]
getList = many1 digit `sepBy1` char ';'

getListTest1 :: Either ParseError [String]
getListTest1 = parse getList "test1 getList" "1;234;56"

getListTest2 :: Either ParseError [String]
getListTest2 = parse getList "test2 getList" "1;234;56;"

getListTest3 :: Either ParseError [String]
getListTest3 = parse getList "test2 getList" "1;;234;56"

-- Right 42
examplePureParser :: Either ParseError Integer
examplePureParser = parse (pure 42) "test always 42" "anything literally"

lettersDigitsIntoPair :: Parsec String u (String, String)
lettersDigitsIntoPair = (,) <$> many1 letter <*> many1 digit

-- Right ("abcd", "123456")
exampleParseLettersDigitsIntoPair :: Either ParseError (String, String)
exampleParseLettersDigitsIntoPair = parse lettersDigitsIntoPair "test parse letters and digits into pair" "abcd123456"

parseVariableName :: Parsec String u String
parseVariableName = string "var" *> spaces *> many1 letter

-- Right "myNewVariable"
exampleParseVariableName1 :: Either ParseError String
exampleParseVariableName1 = parse parseVariableName "test parse 'myNewVariable'" "var myNewVariable = 50;"

-- Right "a"
exampleParseVariableName2 :: Either ParseError String
exampleParseVariableName2 = parse parseVariableName "test parse 'a'" "var       a"

parseVariableNameAndValueTerm :: Parsec String u (String, String)
parseVariableNameAndValueTerm = do
  variableName <- parseVariableName
  spaces
  char '='
  spaces
  valueTerm <- many1 $ satisfy (not . (\c -> isSpace c || c == ';'))
  char ';'
  return (variableName, valueTerm)

-- Right ("kek","10")
exampleParseVariableInit1 :: Either ParseError (String, String)
exampleParseVariableInit1 = parse parseVariableNameAndValueTerm "" "var kek = 10;"

-- Right ("kek","\"Hello_this_is_a_string_without_spaces_inside\"")
exampleParseVariableInit2 :: Either ParseError (String, String)
exampleParseVariableInit2 = parse parseVariableNameAndValueTerm "" "var kek = \"Hello_this_is_a_string_without_spaces_inside\";"

-- Right ("i","(5+3)*2")
exampleParseVariableInit3 :: Either ParseError (String, String)
exampleParseVariableInit3 = parse parseVariableNameAndValueTerm "" "var i = (5+3)*2;"

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces openBr closeBr letterContent = openBr *> letterContent <* closeBr

-- Right "ABC"
testIgnoreBraces :: Either ParseError String
testIgnoreBraces = parse (ignoreBraces (string "[[") (string "]]") (many1 letter)) "" "[[ABC]]DEF"
