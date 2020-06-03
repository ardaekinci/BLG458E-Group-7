import Data.Char

uiOptions = "a) View a Country's Ninja Information \n\
            \b) View All Countries' Ninja Information \n\
            \c) Make a Round Between Ninjas \n\
            \d) Make a Round Between Countries \n\
            \e) Exit"
uiController = do
    putStrLn uiOptions


main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"  