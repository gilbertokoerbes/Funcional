import System.IO 
import Data.Char
palindromo :: IO [Char]
palindromo = do putStr " --- Verificador de palindromos ---"
                putStr "\n --> Digite uma palavra:  "
                x <- getChar
                if x == '\n' then
                    return []
                else
                    do  xs <- getLine
                        if (toLower(x) : removeSpacesAndPont(toLower'(xs))) == reverse (toLower(x) : removeSpacesAndPont(toLower'(xs))) then 
                            return "eh palindromo"
                        else
                            return "nao eh palindromo"


removeSpacesAndPont :: [Char] -> [Char]
removeSpacesAndPont [] = []
removeSpacesAndPont (x:xs)  | isSpace x = removeSpacesAndPont xs
                            | isPunctuation x = removeSpacesAndPont xs
                            | otherwise = x : removeSpacesAndPont xs
toLower' :: [Char] -> [Char]                 
toLower'[]=[]
toLower' (x:xs) = toLower(x):toLower'(xs)


createCIN :: IO [Char]
createCIN = do  putStr " --- Validador de CIN ---"
                putStr "\n --> Informe 8 digitos:  "
                x <- getChar
                if x == '\n' then
                    return []
                else
                    do  xs <- getLine
                        if (length(x:xs)) /= 8  then 
                            return "Quantidade de digitos incorreta"
                        else
                            return (x:xs)

                            
--pega uma cadeia char, passa para getDigit para converter cada char em digit e depois soma
getDigitSum :: [Char] -> Int
getDigitSum [] = 0
getDigitSum (x:xs) = getDigit x + getDigitSum xs
                        

getDigit :: Char -> Int
getDigit c = read [c]

