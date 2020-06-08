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


data Country = Country{countryName :: String, ninjas :: [Ninja], code :: Char, promoted :: Bool} deriving (Show)

displayCountryWarning :: [Country] -> Char -> String
displayCountryWarning state countryCode
    | (promoted country) = (countryName country) ++ " country cannot be included in a fight"
    | otherwise = ""
    where country = (filter (\x -> code x == countryCode) state)!!0

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
fireIndex = 0
lightningIndex = 1
waterIndex = 2
windIndex = 3
earthIndex = 4

displayOptions :: IO()
displayOptions = putStr uiOptions

-- Pattern Matching
-- | The 'isCountryValid' function checks the user input for validation.
-- param1 is String and user input.
isCountryValid :: String -> Bool 
isCountryValid [country] = elem country availableCountries -- Matches on exactly one item for a country with this pattern 
isCountryValid _ = False -- Return False for other inputs

getAvailableNinjas :: [Country] -> [Ninja]
getAvailableNinjas state = allNinjas
    where allNinjas = ninjas (state!!fireIndex) ++ ninjas (state!!lightningIndex) ++ ninjas (state!!waterIndex) ++ ninjas (state!!earthIndex) ++ ninjas (state!!windIndex)

getAvailableNinjasByCountry :: [Country] -> Char -> [Ninja]
getAvailableNinjasByCountry state 'f' = ninjas (state!!fireIndex)
getAvailableNinjasByCountry state 'l' = ninjas (state!!lightningIndex)
getAvailableNinjasByCountry state 'w' = ninjas (state!!waterIndex)
getAvailableNinjasByCountry state 'e' = ninjas (state!!earthIndex)
getAvailableNinjasByCountry state 'n' = ninjas (state!!windIndex)

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

sortNinjas :: [Ninja] -> [Ninja]
sortNinjas []     = []
sortNinjas (x:xs) = sortNinjas smaller ++ [x] ++ sortNinjas larger
  where
    smaller = [a | a <- xs, smallerOrEqualNinja a x]
    larger  = [a | a <- xs, biggerNinja a x]

listNinjas :: [Ninja] -> [String]
listNinjas = map (show)

displayNinjas :: [Ninja] -> String
displayNinjas = unlines . listNinjas

viewNinjasByCountry :: [Country] -> String -> ([Country], String) 
viewNinjasByCountry state countryCode
    | not (isCountryValid countryCode) = (state, invalidCountryInput)
    | otherwise                        = (state, output ++ warning)
    where output = displayNinjas (sortNinjas (getAvailableNinjasByCountry state (toChar countryCode)))
          warning = displayCountryWarning state (toChar countryCode)

displayPromotedNinjas :: [Country] -> String
displayPromotedNinjas state = displayNinjas promotedNinjas
    where promotedNinjas = filter (\x -> status x == "Journeyman") (getAvailableNinjas state)

viewNinjas :: [Country] -> ([Country], String)
viewNinjas state = (state, displayNinjas (sortNinjas (getAvailableNinjas state)))

-- Get the last n element from the lıst
takeEnd :: Int -> [a] -> [a]
takeEnd n = reverse . take n . reverse 

getCountryIndex :: Char -> Int
getCountryIndex x
    | (x == 'f') = 0
    | (x == 'l') = 1
    | (x == 'w') = 2
    | (x == 'n') = 3
    | (x == 'e') = 4

-- | This function removes the loser ninja from the country list.
-- input1: Loser ninja (will be removed)
-- output: Return the updated ninjas of country
updateNinjasOfCountry :: Country -> [Ninja] -> Country
updateNinjasOfCountry c n = Country{countryName=countryName c, ninjas=n, code=code c, promoted=promoted c}

removeNinjaFromCountry :: [Country] -> Ninja -> [Country]
removeNinjaFromCountry state loser = updatedState
    where countryIndexInState = getCountryIndex (country loser)
          loserCountry        = state!!countryIndexInState
          remainingNinjas     = filter (\x -> name x /= name loser) (ninjas loserCountry)
          updatedCountry      = updateNinjasOfCountry loserCountry remainingNinjas
          updatedState        = take countryIndexInState state ++ [updatedCountry] ++ takeEnd (4 - countryIndexInState) state

-- | This function arrange winner score, round and status from the country list.
-- input1: Winner ninja
-- output: Return the updated ninja
updateWinnerNinja :: [Country] -> Ninja -> ([Country], Ninja)
updateWinnerNinja state winner = (updatedState, updatedWinner)
    where countryIndex = getCountryIndex (country winner)
          fromCountry  = state!!countryIndex
          exceptWinner = filter (\x -> name x /= name winner) (ninjas fromCountry)
          -- update the status
          updatedRound = (r winner) + 1
          updatedStatus = if updatedRound == 3 then "Journeyman" else "Junior"
          updatedPromotedStatus = if updatedRound == 3 then True else False
          -- update the winner and state
          updatedWinner = Ninja {name=name winner, country=country winner, status=updatedStatus, 
                                 exam1=exam1 winner, exam2=exam2 winner, ability1=ability1 winner, 
                                 ability2=ability2 winner, r=updatedRound, abilityScore=abilityScore winner, score=(score winner + 10.0)}
          includingWinner = exceptWinner ++ [updatedWinner]
          updatedCountry = Country{countryName=countryName fromCountry, ninjas=includingWinner, code=code fromCountry, promoted=updatedPromotedStatus}
          updatedState = take countryIndex state ++ [updatedCountry] ++ takeEnd (4 - countryIndex) state
    

arrangeRoundResults :: [Country] -> (Ninja, Ninja) -> ([Country], String)
arrangeRoundResults state (winner, loser) = (updatedState, showWinner updatedNinja)
    where (updatedState, updatedNinja) = updateWinnerNinja (removeNinjaFromCountry state loser) winner

selectRandomWinner :: (Ninja, Ninja) -> (Ninja, Ninja)
selectRandomWinner (first, second) 
    | randInt == 0  = (first, second)
    | otherwise     = (second, first)
    where randInt = 0

roundBetweenNinjas :: [Country] -> (Ninja, Ninja) -> ([Country], String)
roundBetweenNinjas state (ninja1, ninja2) 
    | ninja1 > ninja2   = arrangeRoundResults state (ninja1, ninja2)
    | ninja1 < ninja2   = arrangeRoundResults state (ninja2, ninja1)
    | otherwise         = arrangeRoundResults state (winner, loser)
    where (winner, loser) = selectRandomWinner (ninja1, ninja2)

showWinner :: Ninja -> String
showWinner n = "Winner: \"" ++ (name n) ++ ", Round: " ++ (show (r n)) ++ ", Status: " ++ (status n) ++ "\"\n"

findNinjaByNameAndCountry :: [Country] -> String -> Char -> [Ninja]
findNinjaByNameAndCountry state nameOfNinja countryCode = filter (\x -> name x == nameOfNinja) (getAvailableNinjasByCountry state countryCode) 

viewRoundNinjas :: [Country] -> String -> String -> String -> String -> ([Country], String)
viewRoundNinjas state nameOfFirstNinja countryOfFirstNinja nameOfSecondNinja countryOfSecondNinja
    | not (isCountryValid countryOfFirstNinja)  = (state, "Country of first ninja does not exist.\n")
    | not (isCountryValid countryOfSecondNinja) = (state, "Country of second ninja does not exist.\n")
    | length ninjasOfFirstCountry == 0          = (state, "First ninja that you entered not found for given country.\n")
    | length ninjasOfSecondCountry == 0         = (state, "Second ninja that you entered not found for given country.\n")
    | otherwise                                 = roundBetweenNinjas state ((head ninjasOfFirstCountry), (head ninjasOfSecondCountry))
    where ninjasOfFirstCountry = findNinjaByNameAndCountry state nameOfFirstNinja (toChar countryOfFirstNinja)
          ninjasOfSecondCountry = findNinjaByNameAndCountry state nameOfSecondNinja (toChar countryOfSecondNinja)

viewRoundCountries :: [Country] -> String -> String -> ([Country], String)
viewRoundCountries state firstCountryCode secondCountryCode
    | not (isCountryValid firstCountryCode)  = (state, "First country code does not exist.\n")
    | not (isCountryValid secondCountryCode) = (state, "Second country code does not exist.\n")
    | length ninjasOne == 0                  = (state, "There are no ninjas left to fight for first country.\n")
    | length ninjasTwo == 0                  = (state, "There are no ninjas left to fight for second country.\n")
    | warningForFirstCountry /= ""           = (state, warningForFirstCountry)
    | warningForSecondCountry /= ""          = (state, warningForSecondCountry)
    | otherwise                              = roundBetweenNinjas state ((head ninjasOne), (head ninjasTwo))
    where ninjasOne = sortNinjas (getAvailableNinjasByCountry state (toChar firstCountryCode))
          ninjasTwo = sortNinjas (getAvailableNinjasByCountry state (toChar secondCountryCode))
          warningForFirstCountry = displayCountryWarning state (toChar firstCountryCode)
          warningForSecondCountry = displayCountryWarning state (toChar secondCountryCode)

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

constructInitialState :: [Ninja] -> [Country]
constructInitialState allNinjas = initialState
    where fire         = Country{countryName = "fire", ninjas = returnNinjasByCountry 'f' allNinjas, code = 'f', promoted = False}
          lightning    = Country{countryName = "lightning", ninjas=returnNinjasByCountry 'l' allNinjas, code='l', promoted = False}
          earth        = Country{countryName = "earth", ninjas=returnNinjasByCountry 'e' allNinjas,  code='e', promoted = False}
          wind         = Country{countryName = "wind", ninjas=returnNinjasByCountry 'n' allNinjas,  code='n', promoted = False}    
          water        = Country{countryName = "water", ninjas=returnNinjasByCountry 'w' allNinjas,  code='w', promoted = False}
          initialState = [fire, lightning, water, wind, earth]


-- TODO: refactor here, probably duplicated or can be simplified
returnNinjasByCountry :: Char -> [Ninja] -> [Ninja]
returnNinjasByCountry c ninjaList = filter (\x -> country x == c) ninjaList

data Input = ViewNinjas
  | ViewNinjasByCountry String
  | RoundNinja String String String String
  | RoundCountry String String
  | Exit
  | Invalid

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
        _ -> return Invalid


processUserInput :: [Country] -> Input -> ([Country], String)
processUserInput state (ViewNinjasByCountry countryCode) = viewNinjasByCountry state countryCode
processUserInput state (ViewNinjas) = viewNinjas state
processUserInput state (RoundNinja a b c d) = viewRoundNinjas state a b c d
processUserInput state (RoundCountry firstCountryCode secondCountryCode) = viewRoundCountries state firstCountryCode secondCountryCode

mainLoop :: [Country] -> IO()
mainLoop currentState = do
    displayOptions
    input <- readInput

    case input of
        Invalid -> do
            putStrLn "Invalid Action Entered."
            mainLoop currentState
        Exit -> do
            putStrLn (displayPromotedNinjas currentState)
            return ()
        _ -> do
            let (nextState, output) = processUserInput currentState input
            putStrLn output
            mainLoop nextState

main = do  
    -- Read from file and init variables
    content <- readFile "csereport.txt"
    -- split content by newlines and whitespaces, convert it to ninjas data
    let allNinjas = map convertStringListToNinja (map words (lines content))
    -- construct initial state list from ninja list
    let initialState = constructInitialState allNinjas
    mainLoop initialState