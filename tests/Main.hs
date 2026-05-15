import InterpTests
import ParserTests
import Test.Tasty
import Transform.CpserTests
import Transform.UniquifyTests

tests :: IO TestTree
tests = do
  uniquifyTests <- uniquifyTest
  interpTests <- interpTestList
  return $
    testGroup
      "Pref tests"
      [parserTests, interpTests, cpserTestList, uniquifyTests]

main :: IO ()
main = tests >>= defaultMain
