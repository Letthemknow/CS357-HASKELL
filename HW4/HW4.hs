{-
Name: Gautam, Shreeman
Net ID: gautams@unm.edu
-}

zip3 :: 

-- Problem 1, stutter
stutter :: [a] -> [a]
stutter [] = []
stutter (head:tail) = head : head: stutter(tail)

p1tests = [(stutter "Hello World") == "HHeelllloo  WWoorrlldd", (stutter [1,2,3]) == [1,1,2,2,3,3]]


-- Problem 2, compress
compress :: Eq a => [a] -> [a]
compress [] = []
compress [last] = [last]
compress(a:b:as) = if a == b 
                   then compress (b : as) 
                   else a : (compress (b : as))



p2tests = [compress "HHeelllloo WWoorrlldd" == "Helo World",  compress [1,2,2,3,3,3] == [1,2,3]]


-- Problem 3, findIndices
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices _ [] = []
findIndices pred xs = map fst (filter (pred . snd) (zip [0..] xs))

p3tests = [findIndices (< 'a') "AbCdef" == [0,2],findIndices (== 0) [1,2,0,3,0] == [2,4]]


-- Problem 3.5, intersect
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) ys = if (elem x ys) 
                      then x : intersect xs ys 
                      else intersect xs ys

p35tests = [intersect "abc" "cat" == "ac", intersect [1,2,3] [8] == [], intersect [3,2,1] [1,2,3] == [3,2,1]]


-- Problem 4, isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf _ [] = False
isPrefixOf pre as = if as == pre 
                    then True 
                    else isPrefixOf pre (init as)

p4tests = ["foo" `isPrefixOf` "foobar", not $ isPrefixOf [1,2,3] [4,5,6]]


-- Problem 5, isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf _ [] = False
isSuffixOf post as = if as == post 
                     then True 
                     else isSuffixOf post (tail as)

p5tests = ["bar" `isSuffixOf` "foobar", not $ isSuffixOf [1,2,3] [4,5,6]]


-- Problem 6, dot
dot :: [Int] -> [Int] -> Int
dot [] [] = 0
dot (a:as) (b:bs) = (a * b) + (dot as bs)

p6tests = [[0,0,1] `dot` [0,1,0] == 0]


-- Problem 7, increasing
increasing :: (Ord a) => [a] -> Bool
--increasing [] = True
increasing [last] = True
increasing (a:as) = if (a < head as) 
                    then increasing as 
                    else False

p7tests = [increasing "ABCD", not $ increasing [100,99..1]]



-- Problem 8, decimate
decimate :: [a] -> [a]
decimate [] = []
decimate xs = map snd (filter (\(i,x) -> mod i 10 /= 0) (zip [1..] xs))

p8tests = [decimate [1..21] == [1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,21]]



-- Problem 9, encipher
encipher _ _ [] = []
encipher cap small word = [switch letter cap small | letter <- word]
 
{-
   Function that loops over both the lists
   until the replacement of the letter 
   variable is found
-}
switch _ [] _ = '.'
switch _ _ [] = '.'
switch letter (elemOfCap:cap) (elemOfSmall:small) = if letter == elemOfCap
                                                    then elemOfSmall
                                                    else switch letter cap small

p9tests = [encipher ['A'..'Z'] ['a'..'z'] "THIS" == "this"]


-- Problem 10, prefixSum
prefixSum :: (Num a) => [a] -> [a]
prefixSum [x] = [x]
prefixSum (a:b:xs) = a:prefixSum(a+b:xs)

p10tests = [prefixSum [1..10] == [1,3,6,10,15,21,28,36,45,55], prefixSum [2, 5] == [2, 7]]


-- Problem 11, select
select :: (t -> Bool) -> [t] -> [a] -> [a]
select _ _ [] = []
select _ [] _ = []
select pred (a:as) (b:bs) = if pred a 
                            then b : select pred as bs 
                            else select pred as bs

p11tests = [select even [1..26] "abcdefghijklmnopqrstuvwxyz" == "bdfhjlnprtvxz", select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26] == [1,2,3,4,5,6,7]]


-- Problem 12, numbers
numbers :: [Int] -> Int
numbers [] = 0
numbers [oneNum] = oneNum
numbers (x:xs) = numLoop x xs 
    where numLoop updateVal [] = updateVal
          numLoop updateVal (x:xs) = numLoop (updateVal * 10 + x) xs

p12tests = [ numbers [1..4] == 1234]


tests = [p1tests,p2tests,p3tests,p35tests,p4tests,p5tests,p6tests,p7tests,p8tests,p9tests,p10tests,p11tests,p12tests]
likelyCorrect = and $ map and tests