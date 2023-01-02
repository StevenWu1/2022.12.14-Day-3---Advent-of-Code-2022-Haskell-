import System.IO

numbers = zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27..52]

split x = splitAt a x
  where a = quot (length x) 2

commonItem (a:xs) b
 | a `elem` b = a
 | otherwise = commonItem xs b

commonItemH (a, b) = commonItem a b

chartoInt x = head [ b | (a, b) <- numbers, a == x]

commonSum x = chartoInt (commonItemH (split x))

finalSum n = sum (map commonSum n)

commonItem2 :: String -> String -> String -> Char
commonItem2 (a:xs) b c
 | a `elem` b && a `elem` c = a
 | otherwise = commonItem2 xs b c

commonItemH2 a b c = chartoInt (commonItem2 a b c)

breakInto3 [] = []
breakInto3 (a:b:c:xs) = commonItemH2 a b c : breakInto3 xs

finalSum2 x = sum (breakInto3 x)

main = do
  x1 <- readFile "input.txt"
  let x2 = lines x1
  let x3 = take 1 x2
  print (commonItem2 "vJrwpWtwJgWrhcsFMMfFFhFp" "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" "PmmdzqPrVvPwwTWBwg")
  print(finalSum x2)
  print(finalSum2 x2)
