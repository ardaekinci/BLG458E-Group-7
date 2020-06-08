import Data.Char -- Used for toLower
import System.IO -- Flush buffer befor taking input from user

{-
    Constants
    These variables used in the project. 
    Defined as constants.
-}
-- Available Actions
uiOptions = "---------------- Options ----------------\n\
            \a) View a Country's Ninja Information \n\
            \b) View All Countries' Ninja Information \n\
            \c) Make a Round Between Ninjas \n\
            \d) Make a Round Between Countries \n\
            \e) Exit \n"
availableActions = ['a'..'e']
availableCountries = "eElLwWnNfF"            
-- Input and Error texts            
countryInputText = "Enter the country code: "
invalidCountryInput = "Invalid Country entered"
-- Indexes of Countries in The State
fireIndex = 0
lightningIndex = 1
waterIndex = 2
windIndex = 3
earthIndex = 4

{-
    Utils
    These functions are not the main part of the project.
    Used for some basic repeated operations.
-}
-- | This function gets input from user with text
inputWithText   :: String       -- Input string to show to user. 
                -> IO String    -- Return user input.
inputWithText text = do
    putStr text
    hFlush stdout
    getLine

-- | The `toChar` function converts 1 char string to char.
toChar  :: String   -- Takes String as input
        -> Char     -- Return Char 
toChar [chr] = toLower chr
toChar _     = error "Wrong Input Supplied. Input Length must be exactly 1"

-- Get the last n element from the lıst
takeEnd :: Int -> [a] -> [a]
takeEnd n = reverse . take n . reverse 

{-
    Data Types
    Data types that used in the project.
-}
-- Used to store each ninja information
data Ninja = Ninja {
    name:: String,      -- Name of the ninja
    country:: Char,     -- Country code of ninja
    status:: String,    -- Status of ninja (Journeyman, Junior)
    score:: Float,      -- Total score of ninja (exam1 * 0.5 + exam2 * 0.3 + abilityScoreOne + abilityScoreTwo)
    exam1:: Float,      -- Exam one score
    exam2:: Float,      -- Exam two score
    ability1:: String,  -- Name of first ability
    ability2:: String,  -- Name of second ability
    abilityScore:: Int, -- Total ability score of the ninja
    r:: Int             -- Rounds played
    }

-- | Equality of ninjas (Ninja1 == Ninja2). Score and AbilityScore must be equal.
instance Eq Ninja where
  x == y = ((score x) == (score y) && (abilityScore x) ==(abilityScore y))

-- | Comparison of ninjas (Ninja1> Ninja2). If scores are equal compare abilityScores, else compare scores. 
instance Ord Ninja where
  compare a b = if (score a) == (score b) 
      then compare (abilityScore a) (abilityScore b)
      else compare (score a) (score b)

-- | Display ninja. (E.g. "Naruto, Score: 103.5, Status: Junior, Round: 0")
instance Show Ninja where
   show (Ninja name _ status score _ _ _ _ _ r) = name ++ ", Score: " ++ (show score) ++ ", Status: " ++ status ++ ", Round: " ++ (show r)


-- Used to store information of each country.
data Country = Country{
    countryName :: String,  -- Name of country
    ninjas :: [Ninja],      -- Ninja list of country
    code :: Char,           -- Country code
    promoted :: Bool        -- Promoted flag for country (Journeyman)
    }

{-
    Output functions
    These functions used to create output string for the program.
-}

-- | This function create warning message for country if the country has promoted ninja already
displayCountryWarning :: [Country]  -- Input1: Current state of the program. Contains all country
                         -> Char    -- Input2: Country code
                         -> String  -- Output: Generated warning message
displayCountryWarning state countryCode
    | (promoted country) = (countryName country) ++ " country cannot be included in a fight"
    | otherwise = ""
    where country = (filter (\x -> code x == countryCode) state)!!0 -- Get country by code from the current state

-- | This function display promoted ninjas
displayPromotedNinjas :: [Country]  -- Input1: Current state of the program. Contains all country
                         -> String  -- Output: Promoted ninjas
displayPromotedNinjas state = displayNinjas promotedNinjas
    where promotedNinjas = filter (\x -> status x == "Journeyman") (getAvailableNinjas state)

-- | Create string list from ninja list
listNinjas :: [Ninja]       -- Input1: Ninja list
               -> [String]  -- Output: Generated string for each ninja as list
listNinjas = map (show)

-- | Create output string from ninja list 
displayNinjas :: [Ninja]    --Input1: Ninja list
                 -> String  --Output: Output string to display to user.
displayNinjas = unlines . listNinjas

-- | Display available options to user
displayOptions :: IO()
displayOptions = putStr uiOptions

-- | Creates output for winner ninja
showWinner :: Ninja     -- Input1: Winner ninja
              -> String -- Output: String. (E.g. Winner: "Sasuke, Round: 1, Status: Junior")
showWinner n = "Winner: \"" ++ (name n) ++ ", Round: " ++ (show (r n)) ++ ", Status: " ++ (status n) ++ "\"\n"


{-
    Ninja Controller Functions
    These functions are used to control of ninjas.
-}
-- | Get all available ninjas
getAvailableNinjas :: [Country]     -- Input1: Current state of the program. Contains all country
                       -> [Ninja]   -- Output: Ninja list
getAvailableNinjas state = allNinjas
    where allNinjas = ninjas (state!!fireIndex) ++ ninjas (state!!lightningIndex) ++ ninjas (state!!waterIndex) ++ ninjas (state!!earthIndex) ++ ninjas (state!!windIndex)

-- | Get available ninjas by country
getAvailableNinjasByCountry :: [Country]    -- Input1: Current state of the program. Contains all country
                                -> Char     -- Input2: Country code
                                -> [Ninja]  -- Output: Ninja list
getAvailableNinjasByCountry state 'f' = ninjas (state!!fireIndex)
getAvailableNinjasByCountry state 'l' = ninjas (state!!lightningIndex)
getAvailableNinjasByCountry state 'w' = ninjas (state!!waterIndex)
getAvailableNinjasByCountry state 'e' = ninjas (state!!earthIndex)
getAvailableNinjasByCountry state 'n' = ninjas (state!!windIndex)

-- | Update ninjas of country
updateNinjasOfCountry :: Country        -- input1: Country object
                         -> [Ninja]     -- input2: Ninja List
                         -> Country     -- output: Updated country object
updateNinjasOfCountry c n = Country{countryName=countryName c, ninjas=n, code=code c, promoted=promoted c}

-- | find given ninja by using name and country
findNinjaByNameAndCountry :: [Country]  -- input1: Current state of the program. Contains all country
                             -> String  -- input2: Name of ninja
                             -> Char    -- input3: Country code of ninja
                             -> [Ninja] -- output: Founded ninjas
findNinjaByNameAndCountry state nameOfNinja countryCode = filter (\x -> name x == nameOfNinja) (getAvailableNinjasByCountry state countryCode) 

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

-- Pattern Matching
-- | The 'isCountryValid' function checks the user input for validation.
-- param1 is String and user input.
isCountryValid :: String -> Bool 
isCountryValid [country] = elem country availableCountries -- Matches on exactly one item for a country with this pattern 
isCountryValid _ = False -- Return False for other inputs

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

viewNinjasByCountry :: [Country] -> String -> ([Country], String) 
viewNinjasByCountry state countryCode
    | not (isCountryValid countryCode) = (state, invalidCountryInput)
    | otherwise                        = (state, output ++ warning)
    where output = displayNinjas (sortNinjas (getAvailableNinjasByCountry state (toChar countryCode)))
          warning = displayCountryWarning state (toChar countryCode)


viewNinjas :: [Country] -> ([Country], String)
viewNinjas state = (state, displayNinjas (sortNinjas (getAvailableNinjas state)))


getCountryIndex :: Char -> Int
getCountryIndex x
    | (x == 'f') = 0
    | (x == 'l') = 1
    | (x == 'w') = 2
    | (x == 'n') = 3
    | (x == 'e') = 4


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
        score = 0.5 * exam1 + 0.3 * exam2 + fromIntegral abilityOneScore + fromIntegral abilityTwoScore,
        exam1 = exam1,
        exam2 = exam2,
        ability1 = ability1,
        ability2 = ability2,
        abilityScore = abilityOneScore + abilityTwoScore,
        r = 0
    }
    where   exam1 = read (stringList!!2) :: Float
            exam2 = read (stringList!!3) :: Float
            ability1 = stringList!!4
            ability2 = stringList!!5
            abilityOneScore = getAbilityImpact ability1
            abilityTwoScore = getAbilityImpact ability2

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