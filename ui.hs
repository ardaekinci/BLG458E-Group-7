import Control.Monad -- Used for Conditional execution `when for ui controller`
import Data.Char -- Used for toLower

data Ninja = Ninja {
    name:: String, 
    country:: Char, 
    status:: String, 
    score:: Float,
    exam1:: Float, 
    exam2:: Float, 
    ability1:: String, 
    ability2:: String, 
    r:: Int
    }

instance Show Ninja where
   show (Ninja name _ status score _ _ _ _ r) = name ++ ", Score: " ++ (show score) ++ ", Status: " ++ status ++ ", Round: " ++ (show r)


uiOptions = "a) View a Country's Ninja Information \n\
            \b) View All Countries' Ninja Information \n\
            \c) Make a Round Between Ninjas \n\
            \d) Make a Round Between Countries \n\
            \e) Exit\n\
            \Enter the action: "
countryMsg = "Enter the country code: "
invalidCountryInput = "Invalid Country entered"
availableActions = ['a'..'e']
availableCountries = "eElLwWnNfF"
availableNinjas = ["Naruto", "Haruki"] -- will be filled from txt

-- Pattern Matching
-- | The 'isCountryValid' function checks the user input for validation.
-- param1 is String and user input.
isCountryValid :: String -> Bool 
isCountryValid [country] = elem country availableCountries -- Matches on exactly one item for a country with this pattern 
isCountryValid _ = False -- Return False for other inputs

ninja1 = Ninja {name="Naruto", country='f', status="Junior", exam1=40, exam2=75, ability1="Clone", ability2="Summon", r=0, score=133.5}
ninja2 = Ninja {name="Haruki", country='e', status="Journeyman", exam1=40, exam2=75, ability1="Clone", ability2="Summon", r=0, score=75.7}
ninja3 = Ninja {name="Naruto", country='f', status="Junior", exam1=40, exam2=75, ability1="Clone", ability2="Summon", r=0, score=150.2}

fire = [ninja1, ninja3]
earth = [ninja2]

uiController :: IO()
uiController = do
    let uiLoop = do
        putStr uiOptions
        action <- getLine
        selectAction action
        when (action /= "e") uiLoop -- Do not break the loop until the user want to exit
    uiLoop -- Start first loop

getNinjasByCountry :: Char -> [Ninja]
getNinjasByCountry 'f' = fire
getNinjasByCountry 'e' = earth

getNinjas :: [Ninja]
getNinjas = fire ++ earth

listNinjas :: [Ninja] -> [String]
listNinjas = map (show)

displayNinjas :: [Ninja] -> IO()
displayNinjas = putStrLn . unlines . listNinjas

viewNinjasByCountry :: IO() 
viewNinjasByCountry = do
    putStr countryMsg
    country <- getLine
    if isCountryValid country
        then do
            let ninjas = getNinjasByCountry (toLower (country!!0))
            displayNinjas ninjas
        else
            putStrLn invalidCountryInput

viewNinjas :: IO()
viewNinjas = do
    let ninjas = getNinjas
    displayNinjas ninjas

selectAction :: String -> IO()
selectAction "a" = viewNinjasByCountry
selectAction "b" = viewNinjas
selectAction "c" = putStrLn "Round Ninjas"
selectAction "d" = putStrLn "Round Countries"
selectAction "e" = putStrLn "Exit"
selectAction _ = putStrLn "Invalid Action entered"

main = do  
    uiController -- Starts the ui controller