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
-- Impacts of Abilities
clone_impact = 20
hit_impact = 10
lightning_impact = 50
vision_impact = 30
sand_impact = 50
fire_impact = 40
water_impact = 30
blade_impact = 20
summon_impact = 50
storm_impact = 10
rock_impact = 20

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

-- Used to take input from user
data Input = ViewNinjas
  | ViewNinjasByCountry String
  | RoundNinja String String String String
  | RoundCountry String String
  | Exit
  | Invalid


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
-- | Return ninjas by country using all ninjas
returnNinjasByCountry :: Char           -- Input1: Country code
                         -> [Ninja]     -- Input2: All ninjas
                         -> [Ninja]     -- Output: Filtered ninja list
returnNinjasByCountry c ninjaList = filter (\x -> country x == c) ninjaList

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
updateNinjasOfCountry :: Country        -- Input1: Country object
                         -> [Ninja]     -- Input2: Ninja List
                         -> Country     -- output: Updated country object
updateNinjasOfCountry c n = Country{countryName=countryName c, ninjas=n, code=code c, promoted=promoted c}

-- | find given ninja by using name and country
findNinjaByNameAndCountry :: [Country]  -- Input1: Current state of the program. Contains all country
                             -> String  -- Input2: Name of ninja
                             -> Char    -- Input3: Country code of ninja
                             -> [Ninja] -- output: Founded ninjas
findNinjaByNameAndCountry state nameOfNinja countryCode = filter (\x -> name x == nameOfNinja) (getAvailableNinjasByCountry state countryCode) 

-- | Remove loser ninja from country list
removeNinjaFromCountry ::   [Country]       -- Input1: Current state of the program. Contains all country
                            -> Ninja        -- Input2: Loser ninja
                            -> [Country]    -- output: New state of the program.
removeNinjaFromCountry state loser = updatedState
    where countryIndexInState = getCountryIndex (country loser) -- Get country index of loser ninja in the current state
          loserCountry        = state!!countryIndexInState      -- Get country of loser ninja
          remainingNinjas     = filter (\x -> name x /= name loser) (ninjas loserCountry) -- Remove loser ninja from country
          updatedCountry      = updateNinjasOfCountry loserCountry remainingNinjas -- Updated ninjas of country without loser ninja
          updatedState        = take countryIndexInState state ++ [updatedCountry] ++ takeEnd (4 - countryIndexInState) state -- Update country in the state

-- | This function arrange winner score, round and status from the country list.
updateWinnerNinja :: [Country]                  -- Input1: Current state of the program. Contains all country
                      -> Ninja                  -- Input2: Winner ninja
                      -> ([Country], Ninja)     -- Output(Tuple): Returns the new state and winner ninja as updated.
updateWinnerNinja state winner = (updatedState, updatedWinner)
    where countryIndex = getCountryIndex (country winner)   -- find country index of winner ninja
          fromCountry  = state!!countryIndex                -- find country of winner ninja
          exceptWinner = filter (\x -> name x /= name winner) (ninjas fromCountry)  -- remove updated ninja from ninjas
          -- update the status
          updatedRound = (r winner) + 1
          updatedStatus = if updatedRound == 3 then "Journeyman" else "Junior"
          updatedPromotedStatus = if updatedRound == 3 then True else False
          -- update the winner and state
          updatedWinner = Ninja {name=name winner, country=country winner, status=updatedStatus, 
                                 exam1=exam1 winner, exam2=exam2 winner, ability1=ability1 winner, 
                                 ability2=ability2 winner, r=updatedRound, abilityScore=abilityScore winner, score=(score winner + 10.0)}
          includingWinner = exceptWinner ++ [updatedWinner] -- Concatenate updated and current ninjas
          -- Update state and country
          updatedCountry = Country{countryName=countryName fromCountry, ninjas=includingWinner, code=code fromCountry, promoted=updatedPromotedStatus}
          updatedState = take countryIndex state ++ [updatedCountry] ++ takeEnd (4 - countryIndex) state

-- | Manage round results between ninjas. Update winner, remove loser.
arrangeRoundResults :: [Country]                -- Input1: Current state of the program. Contains all country
                        -> (Ninja, Ninja)       -- Input2: (Winner, Loser) ninjas
                        -> ([Country], String)  -- Output: Next state of program and winner message
arrangeRoundResults state (winner, loser) = (updatedState, showWinner updatedNinja)
    where (updatedState, updatedNinja) = updateWinnerNinja (removeNinjaFromCountry state loser) winner

-- | Make a round between ninjas. Select winner and loser.
roundBetweenNinjas :: [Country]                 -- Input1: Current state of the program. Contains all country
                       -> (Ninja, Ninja)        -- Input2: Ninjas that are fighting
                       -> ([Country], String)   -- Output: Next state of program and output message
roundBetweenNinjas state (ninja1, ninja2) 
    | ninja1 > ninja2   = arrangeRoundResults state (ninja1, ninja2)
    | ninja1 < ninja2   = arrangeRoundResults state (ninja2, ninja1)
    | otherwise         = arrangeRoundResults state (winner, loser)
    where (winner, loser) = selectRandomWinner (ninja1, ninja2)

{-
    Ninjas Ranking Functions
    These functions are used to rank ninjas according to CSE rules.
-}
-- | Checks if ninja smaller or equal to other ninja according to CSE rules.
smallerOrEqualNinja :: Ninja    -- Input1: First ninja
                       -> Ninja -- Input2: Second ninja
                       -> Bool  -- Output: Comparison result of ninjas
smallerOrEqualNinja n1 n2 
    | (r n1) < (r n2) = True
    | (r n1) == (r n2) && (score n1) >= (score n2) = True
    | otherwise = False

-- | Checks if ninja has high rank against to other ninja.
biggerNinja ::  Ninja       -- Input1: First ninja
                -> Ninja    -- Input2: Second ninja
                -> Bool     -- Output: Comparison result of ninjas
biggerNinja n1 n2 
    | (r n1) > (r n2) = True
    | (r n1) == (r n2) && (score n1) < (score n2) = True
    | otherwise = False

-- | Sort ninjas according to ranks.
sortNinjas :: [Ninja]       -- Input1: Unsorted ninja list
               -> [Ninja]   -- Output: Sorted ninja list
sortNinjas []     = []  
sortNinjas (x:xs) = sortNinjas smaller ++ [x] ++ sortNinjas larger
  where
    smaller = [a | a <- xs, smallerOrEqualNinja a x]
    larger  = [a | a <- xs, biggerNinja a x]

{-
    Util function for ninjas.
    These functions are used for calculation, validation etc. for ninjas.
-}
-- | Get total ability score of ninja (Sum of two abilities)
getTotalAbilityScore :: String      -- Input1: First ability name
                        -> String   -- Input2: Second ability name
                        -> Int      -- Output: Sum of impacts of abilites
getTotalAbilityScore a1 a2 = (getAbilityImpact a1) + (getAbilityImpact a2)

-- | Get ability impact of ability by name
getAbilityImpact :: String -- Input1: Ability name
                    -> Int -- Output: Ability impact
getAbilityImpact "Clone" = clone_impact
getAbilityImpact "Hit" = hit_impact
getAbilityImpact "Lightning" = lightning_impact
getAbilityImpact "Vision" = vision_impact
getAbilityImpact "Sand" = sand_impact
getAbilityImpact "Fire" = fire_impact
getAbilityImpact "Water" = water_impact
getAbilityImpact "Blade" = blade_impact
getAbilityImpact "Summon" = summon_impact
getAbilityImpact "Storm" = storm_impact
getAbilityImpact "Rock" = rock_impact

-- | This function selects a random winner if scores are equal
selectRandomWinner :: (Ninja, Ninja)    -- Input1: Ninja tuples
                      -> (Ninja, Ninja) -- Output: Return ninjas (Winner, Loser)
selectRandomWinner (first, second) 
    | randInt == 0  = (first, second)
    | otherwise     = (second, first)
    where randInt = 0

{-
    Util function for countries.
    These functions are used for countries objects.
-}

-- | The 'isCountryValid' function checks the user input for validation.
isCountryValid :: String  -- Input1: Country Code
                  -> Bool -- Output: Validation result
isCountryValid [country] = elem country availableCountries -- Matches on exactly one item for a country with this pattern 
isCountryValid _ = False -- Return False for other inputs

-- | Get country index for state by country code
getCountryIndex :: Char     -- Input1: Country code
                   -> Int   -- Output: Country index in the current state
getCountryIndex 'f' = fireIndex
getCountryIndex 'l' = lightningIndex
getCountryIndex 'w' = waterIndex
getCountryIndex 'n' = windIndex
getCountryIndex 'e' = earthIndex

{-
    View Functions.
    These functions used to hande user actions.
-}
-- | Shows sorted ninjas by country 
viewNinjasByCountry :: [Country]                -- Input1: Current state of the program. Contains all country
                        -> String               -- Input2: Country code
                        -> ([Country], String)  -- Output: Next state and output
viewNinjasByCountry state countryCode
    | not (isCountryValid countryCode) = (state, invalidCountryInput)   -- If given country code is not valid print error message
    | otherwise                        = (state, output ++ warning)     
    where output = displayNinjas (sortNinjas (getAvailableNinjasByCountry state (toChar countryCode)))
          warning = displayCountryWarning state (toChar countryCode)

-- | Show sorted ninjas
viewNinjas :: [Country]                 -- Input1: Current state of the program. Contains all country
              -> ([Country], String)    -- Output: Next state and output
viewNinjas state = (state, displayNinjas (sortNinjas (getAvailableNinjas state)))
    
-- | This function handles round between ninjas action. 
viewRoundNinjas :: [Country]                -- Input1: Current state of the program. Contains all country
                    -> String               -- Input2: Name of first ninja
                    -> String               -- Input3: Country code of second ninja
                    -> String               -- Input4: Name of second ninja
                    -> String               -- Input5: Country code of second ninja
                    -> ([Country], String)  -- Output: Next state and output
viewRoundNinjas state nameOfFirstNinja countryOfFirstNinja nameOfSecondNinja countryOfSecondNinja
    -- Validation of inputs
    | not (isCountryValid countryOfFirstNinja)  = (state, "Country of first ninja does not exist.\n")
    | not (isCountryValid countryOfSecondNinja) = (state, "Country of second ninja does not exist.\n")
    | length ninjasOfFirstCountry == 0          = (state, "First ninja that you entered not found for given country.\n")
    | length ninjasOfSecondCountry == 0         = (state, "Second ninja that you entered not found for given country.\n")
    | otherwise                                 = roundBetweenNinjas state ((head ninjasOfFirstCountry), (head ninjasOfSecondCountry))
    where ninjasOfFirstCountry = findNinjaByNameAndCountry state nameOfFirstNinja (toChar countryOfFirstNinja)
          ninjasOfSecondCountry = findNinjaByNameAndCountry state nameOfSecondNinja (toChar countryOfSecondNinja)

-- | This function handles round between countries action. 
viewRoundCountries :: [Country]                 -- Input1: Current state of the program. Contains all country
                       -> String                -- Input2: First country code
                       -> String                -- Input3: Second country code
                       -> ([Country], String)   -- Output: Next state and output
viewRoundCountries state firstCountryCode secondCountryCode
    -- Validation of inputs
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

{-
    File and Initial Read functions.
    These functions are used to construct initial state.
-}
-- | Converts string list to ninja object.
convertStringListToNinja :: [String]  -- Input1: String List
                            -> Ninja  -- Output: Ninja object
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

-- | Construct initial state from all ninjas
constructInitialState :: [Ninja]        -- Input1: All ninjas in the file
                          -> [Country]  -- Output: Initial state of the program
constructInitialState allNinjas = initialState
    where fire         = Country{countryName = "fire", ninjas = returnNinjasByCountry 'f' allNinjas, code = 'f', promoted = False}
          lightning    = Country{countryName = "lightning", ninjas=returnNinjasByCountry 'l' allNinjas, code='l', promoted = False}
          earth        = Country{countryName = "earth", ninjas=returnNinjasByCountry 'e' allNinjas,  code='e', promoted = False}
          wind         = Country{countryName = "wind", ninjas=returnNinjasByCountry 'n' allNinjas,  code='n', promoted = False}    
          water        = Country{countryName = "water", ninjas=returnNinjasByCountry 'w' allNinjas,  code='w', promoted = False}
          initialState = [fire, lightning, water, wind, earth]

-- | Read user input. This function reads user input and returns Input object
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

-- | Process and fetch user input by pattern matchin
processUserInput :: [Country]               -- Input1: Current state
                    -> Input                -- Input2: User input as Input data type
                    -> ([Country], String)  -- Output: Next state and output of the program
processUserInput state (ViewNinjasByCountry countryCode) = viewNinjasByCountry state countryCode
processUserInput state (ViewNinjas) = viewNinjas state
processUserInput state (RoundNinja a b c d) = viewRoundNinjas state a b c d
processUserInput state (RoundCountry firstCountryCode secondCountryCode) = viewRoundCountries state firstCountryCode secondCountryCode

-- | Main loop for user actions. This function creates loop and shows available actions and outputs.
-- | It takes current state and recursively calls by geneerated state.
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

-- | Construct initial state and start the program
main = do  
    -- Read from file and init variables
    content <- readFile "csereport.txt"
    -- split content by newlines and whitespaces, convert it to ninjas data
    let allNinjas = map convertStringListToNinja (map words (lines content))
    -- construct initial state list from ninja list
    let initialState = constructInitialState allNinjas
    mainLoop initialState