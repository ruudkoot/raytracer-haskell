module Test where
import Tests.Input.GML.Parser
import Test.QuickCheck

allTests = [prop_parseOK
           ]

runTests::IO()
runTests = mapM_ quickCheck allTests
