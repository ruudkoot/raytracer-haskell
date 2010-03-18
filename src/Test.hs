module Test where
import Tests.Input.GML.Parser
import Tests.Renderer.Intersections
import Test.QuickCheck

allTests = [ prop_parseOK
           , prop_cylinderIntersect
           , prop_sphereIntersect
           ]

runTests::IO()
runTests = mapM_ quickCheck allTests
