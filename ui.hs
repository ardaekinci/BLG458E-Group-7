import Data.Char

uiOptions = "a) View a Country's Ninja Information \n\
            \b) View All Countries' Ninja Information \n\
            \c) Make a Round Between Ninjas \n\
            \d) Make a Round Between Countries \n\
            \e) Exit"
actionMsg = "Enter the action: "
availableActions = ['a'..'e']
countryMsg = "Enter the country code: "
availableCountries = "eElLwWnNfF"
availableActions = 
uiController = do
    putStrLn uiOptions
    putStr actionMsg
    action <- getLine
    putStrLn "hello"

-- Pattern Matching
isActionValid :: String -> Bool
isActionValid [action] = elem action availableActions -- Matches on exactly one item with this pattern 
isActionValid _ = False -- Return False for other inputs


main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"  