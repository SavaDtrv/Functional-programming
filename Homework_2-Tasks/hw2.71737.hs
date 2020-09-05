import Data.Char
main::IO()
main = do
 print (encodeMessage (zip ['A' .. 'Z'] (['F' .. 'Z'] ++ ['A' .. 'E'])) "The quick brown fox jumps over the lazy dog")
 print (decodeMessage (zip ['A' .. 'Z'] (['F' .. 'Z'] ++ ['A' .. 'E'])) "YMJVZNHPGWTBSKTCOZRUXTAJWYMJQFEDITL")

{-First Task-}
sumNumbersHelp :: String -> Int -> Int -> Int
sumNumbersHelp [] num sum = (num + sum) 
sumNumbersHelp (x:str) num sum
 |x >= '0' && x <= '9' && (head (x:str)) >= '0' && (head (x:str)) <= '9' = 
  sumNumbersHelp str (num*10 + (ord x - ord '0')) sum
 |x >= '0' && x <= '9' && not((head (x:str)) >= '0' && (head (x:str)) <= '9') = 
  sumNumbersHelp str (num + (ord x - ord '0')) sum
 |otherwise = sumNumbersHelp str 0 (sum + num)

sumNumbers :: String -> Int
sumNumbers str = sumNumbersHelp str 0 0

{-Second Task-}
pythagoreanTriples :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriples p = [(a,b,c) | 
 a <- [1.. (p - 3)], b <- [1.. (p - 3)], c <- [1.. (p - 3)], a < b, b < c, a^2 + b^2 == c^2, a + b + c < p]
{-В изразите, a <- [1..(p - 3)], b <- [1..(p - 3)], c <- [1..(p - 3)], интервала на списъка
е до (p - 3), тъй като периметъра трябва да е < p следователно най-малкото число е 1, а най-голямото е
(p - 3) (например 1 + 1 + 47 =49 < 50). В условието имаме проверка за периметъра да е по < p, но сметнах, че
така ще има по-малко проверки компилатора-}

{-Third Task-}
normalizeMessage :: [Char] -> [Char]
normalizeMessage [] = []
normalizeMessage (x:str)
 |isDigit x = error "The message contains digits!"
 |isSymbol x = normalizeMessage str
 |x >= 'a' && x <= 'z' = chr (ord x + (ord 'A' - ord 'a')) : normalizeMessage str
 |otherwise = x : normalizeMessage str
 where
  isSymbol :: Char -> Bool
  isSymbol x = (x < 'A' || x > 'Z') && (x < 'a' || x > 'z')
  isDigit :: Char -> Bool
  isDigit x = x >= '0' && x <= '9'

encodeSymbol :: [(Char,Char)] -> Char -> Char
encodeSymbol [] _ = error "No dictionary found!"
encodeSymbol ((x,y):dct) c = if(c == fst (x,y)) then snd (x,y) else encodeSymbol dct c

encodeMessage :: [(Char,Char)] -> [Char] -> [Char]
encodeMessage dct message = [encodeSymbol dct x | x <- normalizeMessage message]

reverseEncoding :: [(Char,Char)] -> [(Char,Char)]
reverseEncoding [] = error "No dictionary found!"
reverseEncoding dct = [(y,x) | (x,y) <- dct]

decodeMessage :: [(Char,Char)] -> [Char] -> [Char]
decodeMessage dct encoded = [encodeSymbol (reverseEncoding dct) x | x <- encoded]