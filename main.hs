import System.IO
import Data.List
import Data.Char

-- x = [x * 2 | x <- [1..10], x * 2 <= 10]

-- y = reverse (sort x)

-- addMe :: Int -> Int -> Int

-- addMe x y = x + y

-- z = addMe 3 4


-- whatGrade :: Int -> String

-- whatGrade age
--     | (age < 18) = "Can't Vote"
--     | otherwise = "Vote"

-- z = whatGrade 17 


-- calcAvg :: Double -> Double -> Double

-- calcAvg a b = a / b


-- getListItems :: [Int] -> String

-- getListItems [] = "Your list is Empty!"
-- getListItems (x:[]) = "Your list starts with " ++ show x
-- getListItems (x) = "List is " ++ show x


-- getFirstItem :: String -> String
-- getFirstItem [] = "Your string is empty."
-- getFirstItem all@(x:xs) = "First item in " ++ all ++ " is " ++ [x]


-- times4 :: Int -> Int
-- times4 x = x*4

-- listTimes4 = map times4 [1, 2, 3, 4]

-- dbl1To10 = map (\x -> x * 2) [1..10]

data Ninja = Ninja {
    name:: String, 
    country:: Char, 
    status:: Int, 
    exam1:: Float, 
    exam2:: Float, 
    ability1:: String, 
    ability2:: String, 
    r:: Int
    } deriving(Eq, Show)

data Country = Country {
    fire :: [Ninja],
    lightning :: [Ninja],
    water :: [Ninja],
    wind :: [Ninja],
    earth :: [Ninja]
}

-- accessing the index ex_list[0] == ex_list!!0
-- take 1 "Example" == "E"
-- read (x) :: Float, converts string x to Float
convertStringListToNinja :: [String] -> Ninja
convertStringListToNinja l = Ninja {
    name = l!!0,
    country = toLower ((take 1 (l!!1))!!0),
    status = 0,
    exam1 = read (l!!2) :: Float,
    exam2 = read (l!!3) :: Float,
    ability1 = l!!4,
    ability2 = l!!5,
    r = 0
}

-- convertNinjaToTuple :: Ninja -> (String, Char, Int, Float, Float, String, String, Int)
-- convertNinjaToTuple n = (
--     name n,
--     country n,
--     status n,
--     exam1 n,
--     exam2 n,
--     ability1 n,
--     ability2 n,
--     r n)

-- printNinjas :: [Ninja] -> ()
-- printNinjas l = do
--     print(l)

-- should definetely read here
-- https://stackoverflow.com/questions/11229854/how-can-i-parse-the-io-string-in-haskell/11230235#11230235
readCse = do
   content <- readFile "csereport.txt"
   let lines_content = lines content
   let lines_content_w = map words lines_content
   let ninjas_list = map convertStringListToNinja lines_content_w
   return ninjas_list

-- printNinja :: Ninja
printNinja ninja = do
    print ninja

main = do
    print ("x")
    print ("y")
    -- -- reads the file into IO [Char]
    -- content <- readFile "csereport.txt"

    -- -- parsing by '\n' and create list out of it
    -- let content_lines = lines content

    -- -- Each line should be parsed by whitespace
    -- let content_lines = map words content_lines

    -- -- All ninjas into one list
    -- let ninjas_list = map convertStringListToNinja content_lines
    -- x = readCse

    -- print x
    content <- readFile "csereport.txt"
    let lines_content = lines content
    let lines_content_w = map words lines_content
    let ninjas_list = map convertStringListToNinja lines_content_w
