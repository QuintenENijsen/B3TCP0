module Main where

main :: IO ()
main = putStrLn $ show $ digitsToNum [1,3,6,5,4,3,1,6,2] 

--Question 1: words and unwords 
unwordsR :: [String] -> String
unwordsR = tail . unwordsR'
  where
    unwordsR' :: [String] -> String
    unwordsR' [] = ""
    unwordsR' (x:xs) = (' ' : x) ++ unwordsR' xs

unwordsF :: [String] -> String 
unwordsF s = tail $ foldr f "" s
  where
    f :: String -> String -> String
    f x r = (' ' : x) ++ r  

takeWhile' :: (a -> Bool) -> [a] -> ([a], [a])
takeWhile' p xs = takeWhileAcc p xs ([], [])
  where 
    takeWhileAcc :: (a -> Bool) -> [a] -> ([a], [a]) -> ([a], [a])
    takeWhileAcc _ [] (ts, fs) = (ts, fs)
    takeWhileAcc p z@(x:xs) (ts, fs) = 
      if p x 
        then takeWhileAcc p xs ((x:ts), fs)
        else (ts, z ++ fs)


wordsR :: String -> [String]
wordsR [] = []
wordsR xs = 
  let (w, ws) = takeWhile' (\x -> x /= ' ') xs
      (spaces, newWords) = takeWhile' (\x -> x == ' ') ws 
  in w : wordsR newWords

--wordsF :: String -> [String]


--Question 2: foldl
foldlQ :: (b -> a -> b) -> b -> [a] -> b
foldlQ f e [] = e
foldlQ f e (x:xs) = foldlQ f (f e x) xs

--Question 3: Digits
digitsToNum :: [Int] -> Int 
digitsToNum digs = read $ foldlQ (\e x -> e ++ (show x)) "" digs

stringToInt :: String -> Int 
stringToInt = read

--Question 4: Binary trees
data Tree a = Bin (Tree a) (Tree a) | Tip a

information :: Tree a -> [a]
information Tip a = [a]
information (Bin t1 t2) = information t1 ++ information t2

--Question 5: pack

pack :: Tree Int -> String 
pack Tip x = show x
pack (Bin t1 t2) = '{' : pack t1 ++ "," ++ pack t2 ++ "}"

--Question 6: unpack

unpack :: String -> Tree Int 
unpack = undefined
