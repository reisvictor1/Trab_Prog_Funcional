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

printScore[a:b:c:t] = 
    if(a==10) then print "X _|"
    else if(a+b == 10) then print $ show a ++ "/|"
    else print $ show a ++ "" ++ show b
-}

score :: Int -> [Int] -> (IO (), Int)
score sumList [] = (print "", sumList)
score sumList [a] = (print "", a + sumList)
score sumList [a, b] = (print "", sumList + a + b)
score sumList [a, b, c] = (print "", a + b + c + sumList)
score sumList (a:b:c:t) =
    if(a == 10) then
        (print "", sumList + a + b + c + score sumList  (b:c:t))   
    else if(a+b == 10) then
        (print "", sumList + a + b + c + score sumList (c:t))
        
    else 
        (print "", sumList + a + b + score sumList (c:t))


partida :: IO [Int] -> IO Int
partida l = do
    let sumList = 0 
    list <- l
    let finalScore = score sumList list
    return finalScore

main = do
    let ls = getUserInputs
    final <- partida ls
    print final
