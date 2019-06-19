import LexerTests
import ParserTests
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

tests =
  hUnitTestToTests $
  TestList
    [TestLabel "lexer tests" lexerTests, TestLabel "parser tests" parserTests]

main = defaultMain tests
