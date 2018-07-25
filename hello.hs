sumsq :: Int -> Int
sumsq n = foldr (\i acc -> i ^ 2 + acc) 0 [1..n]

length :: [a] -> Int
length = foldl (\acc _-> 1 + acc) 0

minlist :: Ord a => [a] -> a
minlist = foldr1 (\x acc -> if x < acc then x else acc) 

reverse :: [a] -> [a]
reverse = foldr (\x acc -> acc ++ [x]) []

remove :: Eq a => [a] -> [a] -> [a]
remove xs = foldr (\y acc -> if y `elem` xs then acc else y : acc) [] 
