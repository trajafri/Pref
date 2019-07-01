import InterpTests
import LexerTests
import ParserTests
import CpserTests
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

tests =
  hUnitTestToTests $
  TestList
    [TestLabel "lexer tests" lexerTests, 
     TestLabel "parser tests" parserTests,
     TestLabel "eval tests" interpTestList,
     TestLabel "cpser tests" cpserTestList]

main = defaultMain tests
