import System.IO
import Data.List
import Data.Char

data Ninja = Ninja {
    name:: String, 
    country:: Char, 
    status:: String, 
    score:: Float,
    exam1:: Float, 
    exam2:: Float, 
    ability1:: String, 
    ability2:: String, 
    abilityScore:: Int,
    r:: Int
    } deriving (Show)

getAbilityImpact :: String -> Int
getAbilityImpact "Clone" = 20
getAbilityImpact "Hit" = 10
getAbilityImpact "Lightning" = 50
getAbilityImpact "Vision" = 30
getAbilityImpact "Sand" = 50
getAbilityImpact "Fire" = 40
getAbilityImpact "Water" = 30
getAbilityImpact "Blade" = 20
getAbilityImpact "Summon" = 50
getAbilityImpact "Storm" = 10
getAbilityImpact "Rock" = 20

convertStringListToNinja :: [String] -> Ninja
convertStringListToNinja stringList = Ninja {
    name = stringList!!0,
    country = toLower ((take 1 (stringList!!1))!!0),
    status = "Junior",
    score = 0,
    exam1 = read (stringList!!2) :: Float,
    exam2 = read (stringList!!3) :: Float,
    ability1 = stringList!!4,
    ability2 = stringList!!5,
    abilityScore = (getAbilityImpact (stringList!!4)) + (getAbilityImpact (stringList!!5)),
    r = 0
}

data Country = Country{countryName :: String, ninjas :: [Ninja], code :: Char, promoted :: Bool} deriving (Show)

-- should definetely read here
-- https://stackoverflow.com/questions/11229854/how-can-i-parse-the-io-string-in-haskell/11230235#11230235
readCseReport fileName = do
    content <- readFile fileName
    -- split by new line and create string list for each line
    let linesContent = map words (lines content)
    let ninjaList = map convertStringListToNinja linesContent
    let fire = Country{countryName = "fire", ninjas = returnNinjasByCountry 'f' ninjaList, code = 'f', promoted = False}
   
    let lightning = Country{countryName="lightning", ninjas=returnNinjasByCountry 'l' ninjaList, code='l', promoted= False}
    let earth = Country{countryName="earth", ninjas=returnNinjasByCountry 'e' ninjaList,  code='e', promoted= False}
    let wind = Country{countryName="wind", ninjas=returnNinjasByCountry 'n' ninjaList,  code='n', promoted= False}    
    let water = Country{countryName="water", ninjas=returnNinjasByCountry 'w' ninjaList,  code='w', promoted= False}

    let initialState = [fire, lightning, water, wind, earth]
    return initialState

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