main :: IO()
main = do
 --print (increasing (\x -> x * 2) (\x -> x) 0 1)
 --print (increasing (\x -> x * 2) (\x -> x + 1) 1 5)
 --print (distance (\x -> x ^ 2) [(1,1),(2,4),(3,9),(4,15)])
 --print (distance (\x -> x + 1) [(1,1),(2,4),(3,9),(4,15)])
 --print (closest [(\x -> x ^ 2),(\x -> x + 1)] [(1,1),(2,4),(3,9),(4,15)] 5)
 print (replaceAssoc [5,4,2,3] [(1,5),(3,7),(5,9),(7,11),(9,13)])


{-Task one-}
increasing :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
increasing f g a b
 |a == b && isRising a = True
 |a < b && isRising a = increasing f g (a + 1) b
 |otherwise = False
 where
 isRising x = (f.g) x > x && (g.f) x > x
 
{-Task Two-}
{-a-}
distance :: (Int -> Int) -> [(Int,Int)] -> Int
distance f xys = sum[abs(y - f x) | (x,y) <- xys]

{-b-}
closest :: [(Int -> Int)] -> [(Int,Int)] -> (Int -> Int)
closest [f] _ = f
closest (f:g:fs) xys
 |distance f xys <= distance g xys = closest (f:fs) xys
 |otherwise = closest (g:fs) xys

{-Task three-}
replaceAssoc :: [Int] -> [(Int,Int)] -> [Int]
replaceAssoc [] _ = []
replaceAssoc list dict = [replaced x dict | x <- list]
 where
 replaced x [] = x
 replaced x (y:yzs)
  |x == fst y = snd y
  |otherwise = replaced x yzs