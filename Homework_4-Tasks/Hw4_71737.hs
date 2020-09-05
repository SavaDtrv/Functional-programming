main :: IO()
main =
 let bt1 = (Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty)))
     bt2 = (Node 5 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 6 Empty (Node 8 (Node 7 Empty Empty) Empty)) )
 in do
 --print((equalToMax [(\x -> x - 1), (\x -> (x * (x - 1)) + x), (\x -> (x * (x - 1)))]) 1) -- -> 1
 --print((equalToMax [(\x -> x + 1), (\x -> (x * (x - 1)) + x), (\x -> (x * (x - 1)))]) 2) -- -> 4
 --print (numOfNodes [(10,[3,7,12]),(3,[5,8,9]),(7,[11,13]),(12,[6,4]),(8,[1,2])])
 --print (grandchildrenIncreased bt1) -- -> True
 --print (grandchildrenIncreased bt2) -- -> False


{-Task one-}
equalToMax :: [(Int -> Int)] -> (Int -> Int)
equalToMax [f] x = f x
equalToMax (f:f1:fs) x = if(f x < f1 x) then equalToMax (f1:fs) x else equalToMax (f:fs) x

{-  --Tova e drugo reshenie, ako trqbva da vurnem funkciq koqto ne e ot spisuka
equalToMax [] x = (\x -> x * 0) x
equalToMax fs x
 |maxFunc fs x == retFunc x = retFunc x
 |otherwise = (\x -> x * 0) x
 where
 maxFunc [f] x = f x
 maxFunc (f1:f2:fs) x
  |f1 x < f2 x = maxFunc (f2:fs) x
  |otherwise = maxFunc (f1:fs) x
 retFunc = (\x -> x * x)
 -}

{-Task two-}
searchByFather :: Int -> [(Int,[Int])] -> (Int,[Int])
searchByFather node [] = (node,[])
searchByFather node (x:xs)
 | fst x == node = x
 | otherwise = searchByFather node xs

sonsOfFather :: Int -> [(Int,[Int])] -> [Int]
sonsOfFather node tree = snd (searchByFather node tree)

grandfather :: Int -> [(Int,[Int])] -> Int
grandfather node [] = 0
grandfather node tree
 | elem node (sonsOfFather (fst (head tree)) tree) = fst (head tree)
 | otherwise = grandfather node (tail tree)

numOfNodes :: [(Int , [Int])] -> Int
numOfNodes [] = 0
numOfNodes (node:tree)
 |sum (snd (last (node:tree))) == grandfather (fst (last (node:tree))) (node:tree) = 
  1 + numOfNodes (init (node:tree))
 |otherwise = numOfNodes (init (node:tree))

{-Task three-}
data BTree = Empty | Node Int BTree BTree
 deriving (Show,Read)

--Imam i drug variant na tazi zadacha, no ne uspqh da namerq ot kude davashe greshen rezultat

findNodesOnLvl :: BTree -> Int -> [Int]
findNodesOnLvl Empty _ = []
findNodesOnLvl (Node a ltree rtree) 0 = [a]
findNodesOnLvl (Node a ltree rtree) n = findNodesOnLvl ltree (n - 1) ++ findNodesOnLvl rtree (n - 1)

grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Empty = False
grandchildrenIncreased (Node a lt rt)
 |not (null (findNodesOnLvl (Node a lt rt) 2)) = and (map (>a) (findNodesOnLvl (Node a lt rt) 2)) && (grandchildrenIncreased lt) && (grandchildrenIncreased rt)
 |otherwise = True