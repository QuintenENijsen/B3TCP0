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
information (Tip a) = [a]
information (Bin t1 t2) = information t1 ++ information t2

--Question 5: pack

pack :: Tree Int -> String 
pack (Tip x) = show x
pack (Bin t1 t2) = '{' : pack t1 ++ "," ++ pack t2 ++ "}"

--Question 6: unpack

--Find the entire node, by matching the number of opening curly brackets and closing
findNode :: String -> (String, String)
findNode s = 
  let (leftNode, rightNode) = takeNode s 0 ([], [])
  in  (leftNode, tail rightNode)
  where 
    takeNode :: String -> Int -> (String, String) -> (String, String)
    takeNode [] _ vals = vals
    takeNode (x:xs) num (n1, n2)=
      case x of 
        '{'       -> takeNode xs (num + 1) (x:n1, n2)
        '}'       -> if num == 1 
                       then (x:n1, xs ++ n2)
                       else takeNode xs (num - 1) (x:n1, n2)
        otherwise -> takeNode xs num (x:n1, n2)

--Assumption for unpack, no garbage in string.
unpack :: String -> Tree Int 
unpack tree 
  | head tree == '{' = --Behandel als node
      let lrTree = (init . tail) tree
          (lTree, rest) = findNode lrTree
          (rTree, rest') = findNode rest
      in Bin (unpack lTree) (unpack rTree)
  | otherwise        = Tip (read tree)
