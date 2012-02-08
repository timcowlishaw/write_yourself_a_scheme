module WYAScheme.Specs.Parser where
import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit.Base
import WYAScheme.Parser
import Text.ParserCombinators.Parsec.Error (ParseError, errorMessages)

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b  

parserSpecs :: [Spec]
parserSpecs = describe "The parser" [
    it "parses Strings delimited by double quotes" (
      (Right (String "Hello there")) == readExpr "\"Hello there\""
    ),
    it "parses Strings containing an escaped double quote" (
      (Right (String "Hello, so-called \"Haskell Programmer\"")) == readExpr "\"Hello, so-called \\\"Haskell Programmer\\\"\""
    ),
    it "parses Strings containing an escaped tab" (
      (Right (String "Words\tseparated\tby\ttabs")) == readExpr "\"Words\\\tseparated\\\tby\\\ttabs\""
    ),
    it "parses Strings containing an escaped newline" (
      (Right (String "Words\nseparated\nby\nnewlines")) == readExpr "\"Words\\\nseparated\\\nby\\\nnewlines\""
    ),
    it "parses Strings containing an escaped carriage return" (
      (Right (String "Words\rseparated\rby\rcarriage\rreturns")) == readExpr "\"Words\\\rseparated\\\rby\\\rcarriage\\\rreturns\""
    ),
    it "parses Strings containing an escaped backslash" (
      (Right (String "Words\\separated\\by\\backslashes")) == readExpr "\"Words\\\\separated\\\\by\\\\backslashes\""
    ),
    it "parses atoms beginning with a letter" (
      (Right (Atom "hello")) == readExpr "hello"
    ),
    it "parses atoms beginning with a symbol" (
      (Right (Atom "#hello")) == readExpr "#hello"
    ),
    it "parses atoms containing symbols" (
      (Right (Atom "hello?world")) == readExpr "hello?world"
    ),
    it "parses atoms containing numbers" (
      (Right (Atom "hello2world")) == readExpr "hello2world"
    ),
    it "parses the atom #t as a boolean true value" (
      (Right (Bool True)) == readExpr "#t"
    ),
    it "parses the atom #f as a boolean false value" (
      (Right (Bool False)) == readExpr "#f"
    ),
    it "parses integers" (
      (Right (Number 12345)) == readExpr "12345"
    ),
    it "parses binary representations of integers" (
      (Right (Number 12345)) == readExpr "#b11000000111001"
    ),
    it "parses octal representations of integers" (
        (Right (Number 12345)) == readExpr "#o30071"
    ),
    it "parses decimal representations of integers" (
        (Right (Number 12345)) == readExpr "#d12345"
    ),
    it "parses hexadecimal representations of integers" (
        (Right (Number 12345)) == readExpr "#x3039"
    )]
