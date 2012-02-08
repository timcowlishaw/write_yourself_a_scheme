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
      (Right (String "Hello there")) == (readExpr "\"Hello there\"")
    ),
    it "parses atoms beginning with a letter" (
      (Right (Atom "hello")) == (readExpr "hello")
    ),
    it "parses atoms beginning with a symbol" (
      (Right (Atom "#hello")) == (readExpr "#hello")
    ),
    it "parses atoms containing symbols" (
      (Right (Atom "hello?world")) == (readExpr "hello?world")
    ),
    it "parses atoms containing numbers" (
      (Right (Atom "hello2world")) == (readExpr "hello2world")
    ),
    it "parses the atom #t as a boolean true value" (
      (Right (Bool True)) == readExpr "#t"
    ),
    it "parses the atom #f as a boolean false value" (
      (Right (Bool False)) == readExpr "#f"
    ),
    it "parses integers" (
      (Right (Number 12345)) == readExpr "12345"
    )]
