{-
Name: Gautam, Shreeman
Net ID: gautams@unm.edu
-}








































-- Note: you cannot put a = b, where b is some built in function. I.e. no myTakeWhile = takeWhile
-- Problem 1, myTakeWhile
myTakeWhile :: (a-> Bool ) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile pred (head:tail) = if pred head
                               then head:myTakeWhile pred tail
                               else []

p1tests = [myTakeWhile (/= ' ') "This is practice." == "This"]


-- Problem 2, mySpan
mySpan :: (a->Bool) -> [a] -> ([a],[a])
mySpan pred ls = (myTakeWhile pred ls, second pred ls)
    where second :: (a -> Bool) -> [a] -> [a]
          second _ [] = []
          second pred (head:tail) = if pred head
                                    then second pred tail
                                    else head:tail




p2tests = [mySpan (/= ' ') "This is practice." == ("This"," is practice.")]


-- Problem 3, combinations3
combinations3 :: Ord a => [a] -> [[a]]
combinations3 list = [first:second:third:[] | first <- list, second <- list, third <- list, first < second, second < third]

p3tests = [combinations3 "ABCDE" == ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]]























-- Problem 4, runLengthEncode
runLengthEncode :: Eq a => [a] -> [(a, Int)]
runLengthEncode [] = []
runLengthEncode (testElem:restOfTheList) = [(testElem, length sequenceOfTestElem)] ++ runLengthEncode newList
    where (sequenceOfTestElem, newList) = mySpan (== testElem) (testElem:restOfTheList)

p4tests = [runLengthEncode [4,2,2,1,1,1,1,4,4,4,4] == [(4,1),(2,2),(1,4),(4,4)], runLengthEncode "foo" == [('f',1),('o',2)]]











-- Problem 5, runLengthDecode
runLengthDecode :: [(a, Int)] -> [a]
runLengthDecode [] = []
runLengthDecode (firstPair:restOfThePairs) = sequenceFromFirstPair ++ runLengthDecode restOfThePairs
    where sequenceFromFirstPair = replicate (snd firstPair) (fst firstPair)

p5tests = [runLengthDecode [(4,1),(2,2),(1,4),(4,4)] == [4,2,2,1,1,1,1,4,4,4,4], (runLengthDecode $ runLengthEncode "foobar") == "foobar"]






-- Problem 6, splitText
splitText :: Ord a => (a -> Bool) -> [a] -> [[a]]
splitText _ [] = []
splitText pred ls = [myTakeWhile pred ls] ++ splitText pred (restOfTheList pred ls)
    where restOfTheList :: (a -> Bool) -> [a] -> [a]
          restOfTheList _ [] = []
          restOfTheList pred (l:ls) = if pred l
                                      then restOfTheList pred ls
                                      else ls

p6tests = [splitText (/= ' ') "This is practice." == ["This","is","practice."]]






-- Problem 7, encipher
encipher :: Eq a => [a] -> [b] -> [a] -> [b]
encipher capital small word = [small !! index | index <- (map minusOneIndex word)]
    where minusOneIndex word = length (takeWhile (/= word) capital)

p7tests = [encipher ['A'..'Z'] ['a'..'z'] "THIS" == "this",encipher [1..10] (map (\x -> x*x) [1..10]) [10,9..1] == [100,81,64,49,36,25,16,9,4,1],encipher [10,9..0] [10,9..0] [0..10] == [0,1,2,3,4,5,6,7,8,9,10],encipher (['A','C'..'Z'] ++ ['B','D'..'Z']) [1..26] ['A'..'Z'] == [1,14,2,15,3,16,4,17,5,18,6,19,7,20,8,21,9,22,10,23,11,24,12,25,13,26]]


-- Problem 8, goldbach
goldbach :: Int -> [(Int, Int)]
goldbach n = [(x,y) | x <- primes (n `div` 2), y <- primes n, x+y == n]
factors n = [(x,y) | x <- [1..n], y <- [1..n], x*y == n]
primes n = [x | x <- [1..n], ((== 2) . length) (factors x)]

p8tests = [goldbach 6 == [(3,3)], goldbach 9 == [(2,7)]]


-- Problem 9, increasing
increasing :: Ord a => [a] -> Bool
increasing [] = True
increasing [oneElem] = True
increasing (firstElem:secondElem:restOfList) = and [firstElem <= secondElem, increasing (secondElem:restOfList)]

p9tests = [increasing "ABBD", not $ increasing [100,99..1]]


-- Problem 10, select
select :: (t -> Bool) -> [t] -> [a] -> [a]
select pred lsOne lsTwo = [y | (x,y) <- zip lsOne lsTwo, pred x]

p10tests = [select even [1..26] "abcdefghijklmnopqrstuvwxyz" == "bdfhjlnprtvxz", select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26] == [1,2,3,4,5,6,7]]


-- Problem 11, combinations
combinations :: Ord a => Int -> [a] -> [[a]]
combinations k listOfElems = case k of
    0 -> [[]]
    1 -> [[elem] | elem <- listOfElems]
    otherwise -> concat (map (\elem -> [elem:rest | rest <- recur elem]) listOfElems)
    where recur elem = combinations (k-1) (tail (dropWhile (/= elem) listOfElems))

p11tests = [combinations 3 "ABCDE" == ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]]


-- Note: Uncomment the pNtests and in tests below and in tests once you have given a definiton for problem 12

-- Problem 12, ComplexInteger, real, imaginary
data ComplexInteger = ComplexInteger {
    real :: Int,
    imaginary :: Int
}

p12tests = [real (ComplexInteger 1 2) == 1, imaginary (ComplexInteger 2 3) == 3]

-- Problem 13, Eq
instance Eq ComplexInteger where
    (ComplexInteger firstNum secondNum) == (ComplexInteger thirdNum fourthNum) = (firstNum == thirdNum) && (secondNum == fourthNum)

p13tests = [(ComplexInteger 1 2) /= (ComplexInteger 3 4)]


-- Problem 14, Show
instance Show ComplexInteger where
    show (ComplexInteger 0 imaginary) = (show imaginary) ++ "i"
    show (ComplexInteger real 0) = show real
    show (ComplexInteger real imaginary) = (show real) ++ (if imaginary > 0 then "+" else "") ++ (show imaginary) ++ "i"

p14tests = [(show $ ComplexInteger 1 2) == "1+2i", (show $ ComplexInteger 1 0) == "1", (show $ ComplexInteger 0 1) == "1i"]


-- Problem 15, Num
instance Num ComplexInteger where
    (ComplexInteger real1 imaginary1) * (ComplexInteger real2 imaginary2) = ComplexInteger (real1*real2-imaginary1*imaginary2) (real1*imaginary2+real2*imaginary1)

p15tests = [(ComplexInteger 1 2) * (ComplexInteger 3 4) == (ComplexInteger (-5) 10)]


tests = [p1tests,p2tests,p3tests,p4tests,p5tests,p6tests,p7tests,p8tests,p9tests,p10tests,p11tests] -- ++[p12tests,p13tests,p14tests,p15tests]
likelyCorrect = (and [and t | t <- tests], if length tests < 15 then "lacking ComplexInteger tests?" else "")