import Data.Char
import Control.Monad

uiOptions = "a) View a Country's Ninja Information \n\
            \b) View All Countries' Ninja Information \n\
            \c) Make a Round Between Ninjas \n\
            \d) Make a Round Between Countries \n\
            \e) Exit"
actionMsg = "Enter the action: "
countryMsg = "Enter the country code: "
invalidCountryInput = "Invalid Country entered"
availableActions = ['a'..'e']
availableCountries = "eElLwWnNfF"
availableNinjas = ["Naruto", "Haruki"] -- will be filled from txt

{-
getNinjasByCountry :: Char -> IO String
getNinjasByCountry country = do
    return ("Ninjas of ..")

listNinjasByCountry :: IO()
listNinjasByCountry = do
    putStr countryMsg
    country <- getLine
    if (selectAction country)
        then getNinjasByCountry (head country)
        else putStrLn invalidCountryInput

makeRoundBetweenNinjas :: IO() String
makeRoundBetweenNinjas = do
    putStr "Enter the name of the first ninja: "
    firstNinja <- getLine
    putStr "Enter the country code of the first ninja: "
    countryOfFirstNinja <- getLine
    if (isCountryValid countryOfFirstNinja)
        then return invalidCountryInput
    putStrLn "Success"
-}

uiController :: IO()
uiController = do
    let uiLoop = do
        putStrLn uiOptions
        putStr actionMsg
        action <- getLine
        selectAction action
        when (action /= "e") uiLoop -- Do not break the loop until the user want to exit
    uiLoop -- Start first loop

selectAction :: String -> IO()
selectAction "a" = putStrLn "List Ninjas"
selectAction "b" = putStrLn "List All Ninjas"
selectAction "c" = putStrLn "Round Ninjas"
selectAction "d" = putStrLn "Round Countries"
selectAction "e" = putStrLn "Exit"
selectAction _ = putStrLn "Invalid Action entered"

-- Pattern Matching
-- | The 'isCountryValid' function checks the user input for validation.
-- param1 is String and user input.
isCountryValid :: String -> Bool 
isCountryValid [country] = elem country availableCountries -- Matches on exactly one item for a country with this pattern 
isCountryValid _ = False -- Return False for other inputs


main = do  
    uiController -- Starts the ui controller