import           CpserTests
import           InterpTests
import           ParserTests
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList
  [ TestLabel "parser tests" parserTests
  , TestLabel "eval tests"   interpTestList
  , TestLabel "cpser tests"  cpserTestList
  ]

main :: IO ()
main = defaultMain tests
