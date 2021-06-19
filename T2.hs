--  1) Criar função ehPalindromo interativo
import Data.Char (toLower,isAlpha)
import Data.Char
import System.IO

palindromo :: IO [Char]
palindromo = do putStr " --- Verificador de palindromos ---"
                putStr "\n --> Digite uma palavra:  "
                x <- getChar
                if x == '\n' then
                    return []
                else
                    do  xs <- getLine              
                        if (toLower(x) : removeSpacesAndPont(toLower' (xs))) == reverse (toLower(x) : removeSpacesAndPont(toLower' (xs))) then 
                            return "eh palindromo"                 
                        else
                            return "nao eh palindromo"

                        
removeSpacesAndPont :: [Char] -> [Char]
removeSpacesAndPont [] = []
removeSpacesAndPont (x:xs)  | isSpace x = removeSpacesAndPont xs
                            | isPunctuation x = removeSpacesAndPont xs
                            | otherwise = x : removeSpacesAndPont xs


toLower' :: [Char] -> [Char]
toLower' [] = []
toLower' (x:xs) = toLower(x) : toLower'(xs)


-----------------------------------2 -----------------------
createCIN :: IO [Char]
createCIN = do  putStr " --- Validador de CIN ---"
                putStr "\n --> Informe 10 digitos:  "
                x <- getChar
                if x == '\n' then
                    return []
                else
                    do  xs <- getLine
                        if (length(x:xs)) /= 10  then 
                            return "Quantidade de digitos incorreta"
                        else
                            return (x:xs)

                            
--pega uma cadeia char, passa para getDigit para converter cada char em digit e depois soma
getDigitSum :: [Char] -> Int
getDigitSum [] = 0
getDigitSum (x:xs) = getDigit x + getDigitSum xs
                        
getDigit :: Char -> Int
getDigit c = read [c]

validar :: [Char] -> Bool
validar (x:xs) = if getDigitSum (take 8 (x:xs)) == read (reverse (take 2 (reverse xs))) then
                    True
                 else
                    False

-----------------3---------------------------------------------------------
somador :: IO ()
somador = do    putStr "Quantidade de numeros:  "
                n <- readLn
                if n == 0
                    then return ()
                else
                    do
                        numerosLidos <- leNumeros n
                        putStr "Soma dos numeros = "
                        print (numerosSum(numerosLidos))
                        return ()

leNumeros :: (Eq t, Num t) => t -> IO [String]
leNumeros n = do
                if n == 0
                    then return []
                else do putStr ("Escreva um numero: ")
                        numero <- getLine        
                        numeros <- leNumeros (n-1)                                             
                        return ([numero] ++ numeros)

numerosSum [] = 0
numerosSum (x:xs) = (read x :: Int) + numerosSum xs



