import Draw

prop_isSelected :: [Int] -> String -> Int -> Bool -> Bool
prop_isSelected pt colTitle idx expected = (isSelected pt colTitle idx) == expected

isSelected_test_cases :: [([Int], String, Int, Bool)]
isSelected_test_cases = 
    [
        ([0, 1], "To Do", 1, True),
        ([0, 2], "To Do", 3, False),
        ([2, 1], "In Progress", 1, False),
        ([2, 0], "Done", 0, True)
    ]



passedCount :: [Bool]->Int 
passedCount [] = 0
passedCount (x:xs) = 
    if x == True 
    then 1 + passedCount(xs) 
    else passedCount(xs)

testingResults :: [([Int], String, Int, Bool)] -> [Bool]
testingResults = map $ (\(x, y, z,w) -> (prop_isSelected x y z w))  
    
main :: IO ()
main = putStrLn ("Passed " ++ show (passedCount (testingResults isSelected_test_cases)) ++ " out of " ++ show (length isSelected_test_cases) ++ " test cases in total.")
-- main = putStrLn ("Hi there")