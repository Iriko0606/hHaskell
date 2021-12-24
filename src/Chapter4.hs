module Chapter4 where

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty 1ist!"
maximum' [x] = x
maximum' (x:xs) = max x(maximum' xs)

--4.2.1 replicate
rep' :: Int -> a -> [a]
rep' n x
    | n <= 0 = []
    | otherwise = x : rep' (n-1) x

--4.2.2 take
take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

--4.2.3
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--4.2.4
repeat' :: a -> [a]
repeat' n = n: repeat' n

--4.2.5 zip
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

--4.2.6
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = a `elem'` xs

--4.3 クイックソート

quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger
