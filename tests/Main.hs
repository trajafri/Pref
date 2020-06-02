import           Transform.CpserTests
import           Transform.UniquifyTests
import           InterpTests
import           ParserTests
import           Test.Tasty

tests :: IO TestTree
tests = do
  unqTest <- uniquifyTest
  return $ testGroup "Pref tests"
                     [parserTests, interpTestList, cpserTestList, unqTest]

main :: IO ()
main = tests >>= defaultMain
