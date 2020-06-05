import Control.Monad -- Used for Conditional execution `when for ui controller`
import Data.Char -- Used for toLower
import System.IO -- Flush buffer befor taking input from user

-- | The helper method to get input from user with text
inputWithText :: String -> IO String
inputWithText text = do
    putStr text
    hFlush stdout
    getLine

toChar :: String -> Char
toChar [chr] = toLower chr
toChar _     = error "Wrong Input Supplied. Input Length must be exactly 1"

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
    }

instance Eq Ninja where
  x == y = ((score x) == (score y) && (abilityScore x) ==(abilityScore y))

instance Ord Ninja where
  compare a b = if (score a) == (score b) 
      then compare (abilityScore a) (abilityScore b)
      else compare (score a) (score b)

instance Show Ninja where
   show (Ninja name _ status score _ _ _ _ _ r) = name ++ ", Score: " ++ (show score) ++ ", Status: " ++ status ++ ", Round: " ++ (show r)


data Country = Country{countryName :: String, ninjas :: [Ninja], code :: Char, promoted :: Bool}

displayCountryWarning :: Char -> String
displayCountryWarning countryCode
    | (promoted country) = (countryName country) ++ " country cannot be included in a fight"
    | otherwise = ""
    where country = (filter (\x -> code x == countryCode) getCountries)!!0

getTotalAbilityScore :: String -> String -> Int
getTotalAbilityScore a1 a2 = (getAbilityImpact a1) + (getAbilityImpact a2)

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

getCountries :: [Country]
getCountries = [fire, earth, lightning]

uiOptions = "---------------- Options ----------------\n\
            \a) View a Country's Ninja Information \n\
            \b) View All Countries' Ninja Information \n\
            \c) Make a Round Between Ninjas \n\
            \d) Make a Round Between Countries \n\
            \e) Exit \n"
countryInputText = "Enter the country code: "
invalidCountryInput = "Invalid Country entered"
availableActions = ['a'..'e']
availableCountries = "eElLwWnNfF"

diplayOptions :: IO()
displayOptins = putStr uiOptions

-- Pattern Matching
-- | The 'isCountryValid' function checks the user input for validation.
-- param1 is String and user input.
isCountryValid :: String -> Bool 
isCountryValid [country] = elem country availableCountries -- Matches on exactly one item for a country with this pattern 
isCountryValid _ = False -- Return False for other inputs

uiController :: IO()
uiController = do
    let uiLoop = do
        putStr uiOptions
        action <- inputWithText "Enter the action: "
        selectAction action
        when (action /= "e") uiLoop -- Do not break the loop until the user want to exit
    uiLoop -- Start first loop

getAvailableNinjas :: [Ninja]
getAvailableNinjas = (ninjas fire) ++ (ninjas lightning) ++ (ninjas earth)

getAvailableNinjasByCountry :: Char -> [Ninja]
getAvailableNinjasByCountry 'f' = (ninjas fire)
getAvailableNinjasByCountry 'e' = (ninjas earth)
getAvailableNinjasByCountry 'l' = (ninjas lightning)

smallerOrEqualNinja :: Ninja -> Ninja -> Bool
smallerOrEqualNinja n1 n2 
    | (r n1) < (r n2) = True
    | (r n1) == (r n2) && (score n1) >= (score n2) = True
    | otherwise = False

biggerNinja ::  Ninja -> Ninja -> Bool
biggerNinja n1 n2 
    | (r n1) > (r n2) = True
    | (r n1) == (r n2) && (score n1) < (score n2) = True
    | otherwise = False

qSort :: [Ninja] -> [Ninja]
qSort []     = []
qSort (x:xs) = qSort smaller ++ [x] ++ qSort larger
  where
    smaller = [a | a <- xs, smallerOrEqualNinja a x]
    larger  = [a | a <- xs, biggerNinja a x]


getSortedNinjasByCountry :: Char -> [Ninja]
getSortedNinjasByCountry c = qSort (getAvailableNinjasByCountry c)

getSortedNinjas :: [Ninja]
getSortedNinjas = qSort getAvailableNinjas

listNinjas :: [Ninja] -> [String]
listNinjas = map (show)

displayNinjas :: [Ninja] -> IO()
displayNinjas = putStr . unlines . listNinjas

viewNinjasByCountry :: IO() 
viewNinjasByCountry = do
    country <- inputWithText countryInputText
    if isCountryValid country
        then do
            let code = toChar country
            displayNinjas (getSortedNinjasByCountry code)
            putStrLn (displayCountryWarning code)
        else putStrLn invalidCountryInput

viewPromotedNinjas :: IO()
viewPromotedNinjas = displayNinjas promotedNinjas
    where promotedNinjas = filter (\x -> status x == "Journeyman") getAvailableNinjas 

viewNinjas :: IO()
viewNinjas = displayNinjas getSortedNinjas


-- | This function removes the loser ninja from the country list.
-- input1: Loser ninja (will be removed)
-- output: Return the updated ninjas of country
removeNinjaFromCountry :: Ninja -> [Ninja]
removeNinjaFromCountry loser = ninjas fire


-- | This function arrange winner score, round and status from the country list.
-- input1: Winner ninja
-- output: Return the updated ninja
updateWinnerNinja :: Ninja -> Ninja
updateWinnerNinja winner = winner

arrangeRoundResults :: Ninja -> Ninja -> String
arrangeRoundResults winner loser =
    let _ = removeNinjaFromCountry loser
        result = showWinner (updateWinnerNinja winner)
    in result

selectRandomWinner :: Ninja -> Ninja -> (Ninja, Ninja)
selectRandomWinner first second 
    | randInt == 0  = (first, second)
    | otherwise     = (second, first)
    where randInt = 0

roundBetweenNinjas :: Ninja -> Ninja -> String
roundBetweenNinjas ninja1 ninja2 
    | ninja1 > ninja2   = arrangeRoundResults ninja1 ninja2
    | ninja1 < ninja2   = arrangeRoundResults ninja2 ninja1
    | otherwise         = arrangeRoundResults (fst randomNinjas) (snd randomNinjas)
    where randomNinjas = selectRandomWinner ninja1 ninja2

showWinner :: Ninja -> String
showWinner n = "Winner: \"" ++ (name n) ++ ", Round: " ++ (show (r n)) ++ ", Status: " ++ (status n) ++ "\""

findNinjaByNameAndCountry :: String -> Char -> [Ninja]
findNinjaByNameAndCountry nameOfNinja country = filter (\x -> name x == nameOfNinja) (getAvailableNinjasByCountry country) 

displayRoundBetweenNinjas :: String -> String -> String -> String -> String
displayRoundBetweenNinjas nameOfFirstNinja countryOfFirstNinja nameOfSecondNinja countryOfSecondNinja
    | not (isCountryValid countryOfFirstNinja)  = "Country of first ninja does not exist."
    | not (isCountryValid countryOfSecondNinja) = "Country of second ninja does not exist."
    | length firstNinja == 0                    = "First ninja that you entered not found for given country"
    | length secondNinja == 0                   = "Second ninja that you entered not found for given country"
    | otherwise                                 = roundBetweenNinjas (head firstNinja) (head secondNinja)
    where firstNinja = findNinjaByNameAndCountry nameOfFirstNinja (toChar countryOfFirstNinja)
          secondNinja = findNinjaByNameAndCountry nameOfSecondNinja (toChar countryOfSecondNinja)

viewRoundNinjas :: IO()
viewRoundNinjas = do
    nameOfFirstNinja <- inputWithText "Enter the name of the first ninja: "
    countryOfFirstNinja <- inputWithText "Enter the country code of the first ninja: "
    nameOfSecondNinja <- inputWithText "Enter the name of the second ninja: "
    countryOfSecondNinja <- inputWithText "Enter the country code of the second ninja: "
    putStrLn (displayRoundBetweenNinjas nameOfFirstNinja countryOfFirstNinja nameOfSecondNinja countryOfSecondNinja ++ "\n")

displayRoundBetweenCountries :: String -> String -> String
displayRoundBetweenCountries firstCountryCode secondCountryCode
    | not (isCountryValid firstCountryCode)  = "First country code does not exist."
    | not (isCountryValid secondCountryCode) = "Second country code does not exist."
    | length ninjasOne == 0                  = "There are no ninjas left to fight for first country"
    | length ninjasTwo == 0                  = "There are no ninjas left to fight for second country"
    | warningForFirstCountry /= ""           = warningForFirstCountry
    | warningForSecondCountry /= ""          = warningForSecondCountry
    | otherwise                              = roundBetweenNinjas (head ninjasOne) (head ninjasTwo)
    where ninjasOne = getAvailableNinjasByCountry (toChar firstCountryCode)
          ninjasTwo = getAvailableNinjasByCountry (toChar secondCountryCode)
          warningForFirstCountry = displayCountryWarning (toChar firstCountryCode)
          warningForSecondCountry = displayCountryWarning (toChar secondCountryCode)

viewRoundCountries :: IO()
viewRoundCountries = do
    firstCountryCode <- inputWithText "Enter the first country code: "
    secondCountryCode <- inputWithText "Enter the second country code: "
    putStrLn (displayRoundBetweenCountries firstCountryCode secondCountryCode ++ "\n")
    
selectAction :: String -> IO()
selectAction "a" = viewNinjasByCountry
selectAction "b" = viewNinjas
selectAction "c" = viewRoundNinjas
selectAction "d" = viewRoundCountries
selectAction "e" = viewPromotedNinjas
selectAction _ = putStrLn "Invalid Action entered"

data Input = ViewNinjas
  | ViewNinjasByCountry String
  | RoundNinja String String String String
  | RoundCountry String String
  | Exit

readInput :: IO Input
readInput = do
    action <- inputWithText "Enter the action: "
    case action of
        "a" -> do
            countryCode <- inputWithText "Enter the country code: "
            return (ViewNinjasByCountry countryCode)
        "b" -> return ViewNinjas
        "c" -> do
            nameOfFirstNinja <- inputWithText "Enter the name of the first ninja: "
            countryOfFirstNinja <- inputWithText "Enter the country code of the first ninja: "
            nameOfSecondNinja <- inputWithText "Enter the name of the second ninja: "
            countryOfSecondNinja <- inputWithText "Enter the country code of the second ninja: "
            return (RoundNinja nameOfFirstNinja countryOfFirstNinja nameOfSecondNinja countryOfSecondNinja)
        "d" -> do
            firstCountryCode <- inputWithText "Enter the first country code: "
            secondCountryCode <- inputWithText "Enter the second country code: "
            return (RoundCountry firstCountryCode secondCountryCode)
        "e" -> return Exit


mainLoop :: [Country] -> IO()
mainLoop currentState = do
    diplayOptions
    input <- readInput

    case input of
        Exit -> do
            viewPromotedNinjas
            return ()
        _ -> do
            let (nextState, output) = processUserInput currentState input
            putStrLn output
            mainLoop nextState

main = do  
    -- Read from file and init variables
    ninja1 = Ninja {name="Naruto", country='f', status="Junior", exam1=40, exam2=75, ability1="Clone", ability2="Summon", r=0, abilityScore=140, score=133.5}
    ninja2 = Ninja {name="Haruki", country='e', status="Journeyman", exam1=40, exam2=75, ability1="Lightning", ability2="Summon", r=0, abilityScore=140, score=133.5}
    ninja3 = Ninja {name="Hiroshi", country='f', status="Junior", exam1=40, exam2=75, ability1="Water", ability2="Summon", r=0, abilityScore=90, score=150.2}
    ninja4 = Ninja {name="Sasuke", country='l', status="Junior", exam1=40, exam2=75, ability1="Fire", ability2="Summon", r=1, abilityScore=150, score=150.2}
    ninja5 = Ninja {name="five", country='l', status="Junior", exam1=40, exam2=75, ability1="Fire", ability2="Summon", r=1, abilityScore=150, score=140.2}
    ninja6 = Ninja {name="six", country='l', status="Junior", exam1=40, exam2=75, ability1="Fire", ability2="Summon", r=1, abilityScore=150, score=140.2}
    ninja7 = Ninja {name="seven", country='l', status="Junior", exam1=40, exam2=75, ability1="Fire", ability2="Summon", r=2, abilityScore=150, score=130.2}
    
    fire_ninjas = [ninja1, ninja3]
    earth_ninjas = [ninja2]
    lightning_ninjas = [ninja4, ninja5, ninja6, ninja7]

    fire :: Country
    fire = Country{countryName="fire", ninjas=fire_ninjas, code='f', promoted= False}
    lightning :: Country
    lightning = Country{countryName="lightning", ninjas=lightning_ninjas, code='l', promoted= False}
    earth :: Country
    earth = Country{countryName="earth", ninjas=earth_ninjas,  code='e', promoted= True}
    
    let initial_state = [fire, lightning, earth]
    mainLoop initial_state