module Chapter3 where

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "No!"

factorial :: Int -> Int
factorial 0 = 1
factproal n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) =  (x1 + x2, y1 +  y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

--3.1.2
head' :: [a] -> a
head' [] = error "Can't call head on an epty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell(x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "this list is log. The first two elements are: " ++ show x ++ " and " ++ show y

badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z

--3.1.3
firstLetter :: String -> String
firstLetter "" = "Empty string whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--3.2
bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal."
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, coungratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  |  otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

--3.3: where?!
badGreeting :: String
badGreeting = "Oh! pfft. Its you."
niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"
greet :: String -> String
greet "Juan" = niceGreeting ++ "Juan!"
greet "Fernando" = niceGreeting ++ "Fernando!"
greet name = badGreeting ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
  where bmi weight height = weight / height ^ 2

--3.4: let It Be

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea


--3.4.1
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

--3.5: case式
head2' :: [a] -> a
head2' [] = error "No head for empty lists!"
head3' (x:_) = x
head4' ::[a] -> a
head4' xs = case xs of [] -> error "No head for empty lists!"
--                (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list"
                                               xs -> "a longer list."
