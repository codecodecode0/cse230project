import Draw
import Events
import Defs
import Data.Text (pack)

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

moveRightTestCases :: [([TaskData], [TaskData], [TaskData], Int, Int, [Int])]
moveRightTestCases = 
    [
        ([TaskData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "Viraj") Low],
        [TaskData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        [TaskData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low], 
        0, 0, [1, 2, 1]),

        ([TaskData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "Viraj") Low],
        [TaskData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        [TaskData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low], 
        1, 0, [2, 0, 2]),

        ([TaskData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "Viraj") Low],
        [TaskData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        [TaskData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        2, 0, [2, 1, 1])
    ]

moveLeftTestCases :: [([TaskData], [TaskData], [TaskData], Int, Int, [Int])]
moveLeftTestCases = 
    [
        ([TaskData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "Viraj") Low],
        [TaskData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        [TaskData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low], 
        0, 0, [2, 1, 1]),

        ([TaskData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "Viraj") Low],
        [TaskData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        [TaskData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low], 
        1, 0, [3, 0, 1]),

        ([TaskData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "Viraj") Low],
        [TaskData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        [TaskData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        2, 0, [2, 2, 0])
    ]

deleteTestCases :: [([TaskData], [TaskData], [TaskData], Int, Int, [Int])]
deleteTestCases = 
    [
        ([TaskData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "Viraj") Low],
        [TaskData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        [TaskData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low], 
        0, 0, [1, 1, 1]),

        ([TaskData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "Viraj") Low],
        [TaskData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        [TaskData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low], 
        1, 0, [2, 0, 1]),

        ([TaskData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "Viraj") Low],
        [TaskData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        [TaskData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low],
        2, 0, [2, 1, 0])
    ]

prop_moveRight :: [TaskData] -> [TaskData] -> [TaskData] -> Int -> Int -> [Int] -> Bool
prop_moveRight todos progs dones cpx cpy exp = do
    let mr = moveToRight todos progs dones cpx cpy
    (exp !! 0) == length (mr !! 0) && (exp !! 1) == length (mr !! 1) && (exp !! 2) == length (mr !! 2)

prop_moveLeft :: [TaskData] -> [TaskData] -> [TaskData] -> Int -> Int -> [Int] -> Bool
prop_moveLeft todos progs dones cpx cpy exp = do
    let mr = moveToLeft todos progs dones cpx cpy
    (exp !! 0) == length (mr !! 0) && (exp !! 1) == length (mr !! 1) && (exp !! 2) == length (mr !! 2)

prop_deleteTask :: [TaskData] -> [TaskData] -> [TaskData] -> Int -> Int -> [Int] -> Bool
prop_deleteTask todos progs dones cpx cpy exp = do
    let mr = deleteTask todos progs dones cpx cpy
    (exp !! 0) == length (mr !! 0) && (exp !! 1) == length (mr !! 1) && (exp !! 2) == length (mr !! 2)


passedCount :: [Bool]->Int 
passedCount [] = 0
passedCount (x:xs) = 
    if x == True 
    then 1 + passedCount(xs) 
    else passedCount(xs)

testingResults :: [([Int], String, Int, Bool)] -> [Bool]
testingResults = map $ (\(x, y, z,w) -> (prop_isSelected x y z w))

testMoveRight :: [([TaskData], [TaskData], [TaskData], Int, Int, [Int])] -> [Bool]
testMoveRight = map $ (\(x, y, z,w, u, v) -> (prop_moveRight x y z w u v))

testMoveLeft :: [([TaskData], [TaskData], [TaskData], Int, Int, [Int])] -> [Bool]
testMoveLeft = map $ (\(x, y, z,w, u, v) -> (prop_moveLeft x y z w u v))
    
testDelete :: [([TaskData], [TaskData], [TaskData], Int, Int, [Int])] -> [Bool]
testDelete = map $ (\(x, y, z,w, u, v) -> (prop_deleteTask x y z w u v))

main :: IO ()
main = do
    putStrLn ("Test 1: Passed " ++ show (passedCount (testingResults isSelected_test_cases)) ++ " out of " ++ show (length isSelected_test_cases) ++ " test cases in total.")
    putStrLn ("Test 2: Passed " ++ show (passedCount (testMoveRight moveRightTestCases)) ++ " out of " ++ show (length moveRightTestCases) ++ " test cases in total.")
    putStrLn ("Test 3: Passed " ++ show (passedCount (testMoveLeft moveLeftTestCases)) ++ " out of " ++ show (length moveLeftTestCases) ++ " test cases in total.")
    putStrLn ("Test 4: Passed " ++ show (passedCount (testDelete deleteTestCases)) ++ " out of " ++ show (length deleteTestCases) ++ " test cases in total.")
-- main = putStrLn ("Hi there")