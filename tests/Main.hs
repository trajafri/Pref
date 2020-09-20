import           CpserTests
import           InterpTests
import           ParserTests
import           Test.Tasty

tests :: TestTree
tests = testGroup "Pref tests" [parserTests, interpTestList, cpserTestList]

main :: IO ()
main = defaultMain tests
