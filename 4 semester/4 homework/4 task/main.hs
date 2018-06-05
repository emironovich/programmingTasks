module Main where

data Entry = Entry {name::String, phoneNumber::String} deriving (Eq,Show)

main = myloop []

myloop :: [Entry] -> IO ()
myloop phonebook = do
    putStrLn "Enter command number"
    putStrLn  "0 - exit"
    putStrLn  "1 - add entry (name and phone number)"
    putStrLn  "2 - find phone number by name"
    putStrLn  "3 - find name by phone number"
    putStrLn  "4 - save current data to file"
    putStrLn  "5 - read data from file"
    putStrLn  "6 - print"
    command <- getLine
    case command of
        "0" -> do
            return ()
        "1" -> do
            putStrLn "Enter the entry you want to add"
            entryStr <- getLine
            myloop $ addEntry (makeEntry $ words entryStr) phonebook 
        "2" -> do
            putStrLn "Enter the name"
            name <- getLine
            putStrLn $ findNumberByName phonebook name 
            myloop phonebook
        "3" -> do
            putStrLn "Enter the phone number"
            number <- getLine
            putStrLn $ findNameByNumber phonebook number 
            myloop phonebook
        "4" -> do
            putStrLn "Enter the filepath"
            filepath <- getLine
            writePhonebook filepath phonebook
            myloop phonebook   
        "5" -> do
            putStrLn "Enter the filepath"
            filepath <- getLine
            content <- readFile filepath
            myloop $ addManyEntries (map (makeEntry . words) (lines content)) phonebook
        "6" -> do
            printAll phonebook
            myloop phonebook
            

makeEntry :: [String] -> Entry
makeEntry xs = Entry (foldr (\x y -> y ++ " " ++ x) "" (tail $ reverse xs)) (head $ reverse xs)


addEntry :: Entry -> [Entry] -> [Entry]
addEntry (Entry n p) (e : es) | n < (name e) = (Entry n p) : e : es
                              | otherwise = e : (addEntry (Entry n p) es)
addEntry e [] = [e]
            
findNumberByName :: [Entry] -> String -> String
findNumberByName ((Entry n p):es) name | name == n = p
                                       | name < n = findNumberByName es name
                                       | otherwise = "There is no number for this name"
findNumberByName [] _ = "There is no number for this name"

findNameByNumber :: [Entry] -> String -> String
findNameByNumber ((Entry n p):es) number | number == p = n
                                         | otherwise = findNameByNumber es number
findNameByNumber [] _ = "There is no name for this number"

addManyEntries :: [Entry] -> [Entry] -> [Entry]
addManyEntries (e:es) original = addManyEntries es (addEntry e original)
addManyEntries [] es = es

writePhonebook :: FilePath -> [Entry] -> IO()
writePhonebook filepath ((Entry n p) : es) = do
        appendFile filepath (n ++ " " ++ p ++ "\n")
        writePhonebook filepath es
writePhonebook _ [] = return()

printAll :: [Entry] -> IO()
printAll ((Entry n p) : es) = do
    putStrLn $ n ++ " " ++ p
    printAll es
printAll [] = return()
    