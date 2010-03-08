import Distribution.Simple 


hooks = simpleUserHooks { runTests = \_ _ _ _-> putStrLn "Running the test suite" }

main = defaultMainWithHooks hooks
