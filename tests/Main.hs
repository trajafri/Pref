import CpserTests
import InterpTests
import LexerTests
import ParserTests
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

tests :: [Test.Framework.Test]
tests =
  hUnitTestToTests $
  TestList
    [ TestLabel "lexer tests" lexerTests
    , TestLabel "parser tests" parserTests
    , TestLabel "eval tests" interpTestList
    , TestLabel "cpser tests" cpserTestList
    ]

main :: IO ()
main = defaultMain tests
