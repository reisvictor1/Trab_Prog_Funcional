import Data.List
import Text.Read
import Text.XHtml (input)

getUserInputs :: IO [Int]
getUserInputs = do
    inputLine <- getLine
    let ls = words inputLine
    let intList = map read ls :: [Int]
    return intList

{-
printScore[a:b:c:t] = do
    let r_a = show a
    let r_b = show b
    if(a==10) then print "X _|"
    else if(a+b == 10) then print r_a $ print "/|"
    else print r_a $ print r_b
-}

score :: Int -> [Int] -> Int
score sumList [] = sumList
score sumList [a] = a + sumList
score sumList [a, b] = sumList + a + b
score sumList [a, b, c] = a + b + c + sumList
score sumList (a:b:c:t)
  | a == 10 = sumList + a + b + c + score sumList (b:c:t)
  | a+b == 10 = sumList + a + b + c + score sumList (c:t)
  | otherwise = sumList + a + b + score sumList (c:t)


partida :: [Int] -> Int
partida l = do
    let sumList = 0
    let finalScore = score sumList l
    print finalScore
    return finalScore

main = do
    let ls = getUserInputs
    printList <- ls
    print printList
