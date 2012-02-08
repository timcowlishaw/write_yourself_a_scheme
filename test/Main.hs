module Main where
import Test.Hspec
import WYAScheme.Specs.Parser

specs :: [Spec]
specs = concat [parserSpecs]

main :: IO ()
main = hspec specs >> return ()
