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

fire :: [Ninja]
fire = []
lightning :: [Ninja]
lightning = []
water :: [Ninja]
water = []
wind :: [Ninja]
wind = []
earth :: [Ninja]
earth = []

convertStringListToNinja :: [String] -> Ninja
convertStringListToNinja stringList = Ninja {
    name = stringList!!0,
    country = toLower ((take 1 (stringList!!1))!!0),
    status = 0,
    exam1 = read (stringList!!2) :: Float,
    exam2 = read (stringList!!3) :: Float,
    ability1 = stringList!!4,
    ability2 = stringList!!5,
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
main fileName = do
   -- read file
   content <- readFile fileName
   -- split by new line and create string list for each line
   let linesContent = map words (lines content)
   let ninjaList = map convertStringListToNinja linesContent

   let fire = returnNinjasByCountry 'f' ninjaList
   return fire

returnNinjasByCountry :: Char -> [Ninja] -> [Ninja]
returnNinjasByCountry c ninjaList = filter (\x -> country x == c) ninjaList

-- printNinja :: Ninja
-- printNinja ninja = do
--     print ninja

-- main = do
--     let ninjaList = readInputFile "csereport.txt"
--     return (returnNinjasByCountry 'f' ninjaList)

-- x = readInputFile "csereport.txt"
-- ninjaList = convertStringListToNinja x