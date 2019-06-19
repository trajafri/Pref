import InterpTests
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

tests = hUnitTestToTests $ interpTestList

main = defaultMain tests
