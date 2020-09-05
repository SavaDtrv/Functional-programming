main :: IO()
main = do
 print(sumPrimes 9 3)

{-1-}
discriminant :: Double -> Double -> Double -> Double
discriminant a b c = b^2 - 4*a*c

solveQuadratic :: Double -> Double -> Double -> (Double, Double)
solveQuadratic a b c = 
 if ((discriminant a b c) < 0) then error "Invalid discriminant" 
 else (((-b) + sqrt(discriminant a b c)) / (2*a), ((-b) - sqrt(discriminant a b c)) /(2*a))

{-2-}
helpDiv :: Integer -> Integer -> Integer -> Integer 
helpDiv n d count 
 |d > div n 2 = count
 |mod n d == 0 = (helpDiv n (d+1) (count+1))
 |otherwise = (helpDiv n (d+1) count)

countDivisors :: Integer -> Integer
countDivisors n = (helpDiv n 1 1) {-Vsqko chislo se deli na sebe si-}

helpPrimes :: Integer -> Integer -> Integer -> Integer
helpPrimes n k sum
 |k == 0 = sum
 |(countDivisors n) == 2 = (helpPrimes (n+1) (k-1) (sum+n))
 |otherwise = (helpPrimes (n+1) k sum)
 
sumPrimes :: Integer -> Integer -> Integer
sumPrimes n k = (helpPrimes n k 0)

{-3-}
helpRev :: Integer -> Integer -> Integer
helpRev n res = if (n < 10) then (res*10 + n) 
 else (helpRev (div n 10) (res*10 + mod n 10)) 

reverseDigits :: Integer -> Integer
reverseDigits n = (helpRev n 0)

isPalindrome :: Integer -> Bool
isPalindrome n = if (n == (reverseDigits n)) then True else False

helpPal :: Integer -> Integer -> Integer -> Integer
helpPal a b count
 |a > b = error "Invalid interval."
 |(a == b) && ((isPalindrome a) == False) = count
 |(a == b) && ((isPalindrome a) == True) = (count+1)
 |(isPalindrome a) == True = (helpPal (a+1) b (count+1))
 |otherwise = (helpPal (a+1) b count)
 
countPalindromes :: Integer -> Integer -> Integer
countPalindromes a b = (helpPal a b 0)

{-4-}
helpTaylor :: Double -> Double -> Double -> Double -> Double
helpTaylor n x k t 
 |k > n = t
 |otherwise = t + (helpTaylor n x (k+1) ((((-1)**k)*(x**(k+1))) / (k+1)))

taylorLog :: Double -> Double -> Double
taylorLog n x = (helpTaylor n x 0 0)








