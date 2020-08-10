{-
Name: Gautam, Shreeman
Net ID: gautams@unm.edu
-}


-- Problem 1, bits2num
bits2num :: Num a => [Char] -> a
bits2num ls = helper ls (length ls - 1) -- Construct a helper method and give length n - 1 to be used below
        where helper [] _ = 0   
              helper (l:ls) len = if (read[l] :: Int) == 1 -- If character is 1
                                  then 2^len + helper ls (len-1)  --then multiply by 2^(n - 1)
                                  else helper ls (len-1)          --else continue recursing

p1tests = [bits2num "1011000" == 88]


-- Problem 2, num2bits
num2bits :: Integral a => a -> [Char]
num2bits n = "0" ++ reverse (helper n) --Multiply from the start and then reverse the bits that were achieved, add "0" to satisfy question requirement
      where helper 0 = "0"
            helper 1 = "1"
            helper n = if (n `mod` 2) == 0 -- If even number 
                       then "0" ++ helper (n `div` 2) --then append 0, recursively divide by 2
                       else "1" ++ helper (n `div` 2) --else append 1, recursively divide by 2

p2tests = [num2bits 87783 == "010101011011100111"]


-- Problem 3, variance
variance :: (Num a, Fractional a) => [a] -> a
variance ls = (sum (map (\l -> diff l average) ls)) / len -- using map do (x - mean)^2 for every list element, sum it, divide by length
         where diff l average = (l - average)^2 --  do (x - mean)^2
               average = sum ls / len -- find mean 
               len = fromIntegral $ length ls -- find length of list

p3tests = [variance [1..10] == 8.25]


-- Problem 4, difference
difference :: Eq a => [a] -> [a] -> [a]
difference xs ys = makeSet (notASet xs ys) -- find a non-set, that is, a list with duplicated items, send it to makeSet to make the list a set
           where notASet [] ys = [] 
                 notASet (x:xs) ys = if (x `elem` ys) -- if x is a member of list ys
                                 then notASet xs ys -- then recurse with list xs and list ys
                                 else x : (notASet xs ys) -- otherwise cons x into the recursion
{-
  make the list a set
-}
makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x:ls) = if (elem x ls) then x : (makeSet (helper x ls)) else x : (makeSet ls)
    where helper x [] = []
          helper x (a:ls) = if (a == x) then helper x ls else a : helper x ls

p4tests = [difference "ABCD" "AD" == "BC",difference "ABCDCBA" "AD" == "BC"]


-- Problem 5, splits
splits ::  Ord a => [a] -> [([a], [a])]
splits ls = map (\l -> (difference ls l, l)) combinations -- compare combinations list with given list, find difference, put it in a tuple
    where combinations = filter (\l -> (length l > 0) && (length l < length ls)) (powerset ls) --weed out the empty and the full set

{-
  find every combination possible of list
-}
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (l:ls) = (map (l:) (powerset ls)) ++ powerset ls

p5tests = [splits "abc" == [("c","ab"),("b","ac"),("bc","a"),("a","bc"),("ac","b"),("ab","c")]]


-- Problem 6, argmin
argmin ::  (Ord a) => (t -> a) -> [t] -> t
argmin _ [x] = x
argmin f xs = snd $ foldr compare (head funcPair) funcPair -- use head of tuple and compare it to all of the list using foldr, get tuple with least function value, use snd to get the list element from tuple
           where funcPair = map (\l -> (f l, l)) xs -- make a tuple of function applied to list element with list element
                 compare firstPair secondPair = if (fst firstPair) < (fst secondPair) -- tuple with least function value gets returned due to foldr
                                                then firstPair
                                                else secondPair
p6tests = [argmin length ["ABC","EF","GHIJ","K"] == "K"]


data Htree a = HLeaf Double a | HFork Double [a] (Htree a) (Htree a) deriving (Show, Eq)
-- Problem 7, bogus

instance (Ord a) => Ord (Htree a) where
    (HLeaf x _) < (HLeaf y _) = x < y
    (HLeaf x _) < (HFork y _ _ _) = x < y
    (HFork x _ _ _) < (HLeaf y _) = x < y
    (HFork x _ _ _) < (HFork y _ _ _) = x < y
    (HLeaf x _) <= (HLeaf y _) = x <= y
    (HLeaf x _) <= (HFork y _ _ _) = x <= y
    (HFork x _ _ _) <= (HLeaf y _) = x <= y
    (HFork x _ _ _) <= (HFork y _ _ _) = x <= y

-- encode character using Huffman coding tree
encode (HFork _ _ (HLeaf _ l) (HLeaf _ r)) c = if c == l then "0" else "1"
encode (HFork _ _ (HLeaf _ l) v@(HFork _ rs _ _)) c =
    if c == l then "0" else '1':(encode v c)
encode (HFork _ _ u@(HFork _ ls _ _) v@(HLeaf _ r)) c =
    if c == r then "1" else '0':(encode u c)
encode (HFork _ _ u@(HFork _ ls _ _) v@(HFork _ rs _ _)) c =
    if c `elem` ls then '0':(encode u c) else '1':(encode v c)

-- decode message using Huffman coding tree
decode t [] = []
decode t (x:xs) = loop t (x:xs)
    where loop (HLeaf _ l) xs = l:(decode t xs)
          loop (HFork _ _ u v) ('0':xs) = loop u xs
          loop (HFork _ _ u v) ('1':xs) = loop v xs


bogus :: Ord a => [(Double, a)] -> Htree a
bogus = undefined

p7tests = let xs = [(0.30,'e'), (0.14,'h'), (0.1,'l'), (0.16,'o'), (0.05,'p'), (0.23,'t'), (0.02,'w')] in [(decode (bogus xs) $ concatMap (encode (bogus xs)) "hello") == "hello", concatMap (encode (bogus xs)) "hello" /= concatMap (encode (bogus xs)) "oellh"]

-- The [1..n] is just for the function to know that the function
-- happens 4 times, no use beyond that. func when applied to arg
-- gives a reduced string and const ensures that the reduced
-- string is the new value. After doing it 4 times, we get "EFGH"
-- Problem 8, church
church :: Int -> (a -> a) -> a -> a
church 0 _ arg = arg
church n func arg = foldr (const func) arg [1..n]

p8tests = [church 4 tail "ABCDEFGH" == "EFGH", church 100 id 9001 == 9001]


data Btree a = BLeaf a | BFork (Btree a) (Btree a) deriving (Show, Eq)
-- Problem 9, trees

{-
  Helper method that generates graph combinations for any given leaflist
  map comes in handy, for instance, "A" and "BC" would go to the third line
  in this method, "A" would be a leaf on the left, and because of a map and 
  recursive call which calls splits, which splits "BC" into "B" and "C"
  & "C" and "B" respectively, "A" becomes a leaf on the left, attached to
  two separate forks in the right, of two separate trees, where "B" and "C"
  swap positions as leaves.
-}
buildTreeVariants :: (Ord a) => ([a], [a]) -> [Btree a]
buildTreeVariants ([oneLeaf], [twoLeaf]) = [BFork (BLeaf oneLeaf) (BLeaf twoLeaf)] --If leaflist has two elements
buildTreeVariants (restOfLeaves, [oneLeaf]) = map (\restOfLeaves' -> BFork restOfLeaves' (BLeaf oneLeaf)) (trees restOfLeaves) -- if leaflist has multiple leaves on the left and a single leaf on the right
buildTreeVariants ([oneLeaf], restOfLeaves) = map (\restOfLeaves' -> BFork (BLeaf oneLeaf) restOfLeaves') (trees restOfLeaves) -- if leaflist has multiple leaves on the right and a single leaf on the left
buildTreeVariants (oneLeafSet, twoLeafSet) = concatMap (\twoLeafSet' -> map (\oneLeafSet' -> BFork oneLeafSet' twoLeafSet') (trees oneLeafSet)) (trees twoLeafSet) -- if leaflist has multiple leaves on both sides

-- Solved this problem using splits because it determines what goes on the left and right of a tree
-- For instance splits "ABC" = "C" and "AB", "B" and "AC", "BC" and "A"
--                             "A" and "BC", "AC" and "B", "AB" and "C"
-- for "C" and "AB", it would be a left "C" leaf and a fork on the right, with "A" and "B" changing positions as leaves
trees :: Ord a => [a] -> [Btree a] 
trees leafList = concatMap buildTreeVariants (splits leafList)
 
p9tests = [(trees "ABCDE") !! 114 == BFork (BLeaf 'E') (BFork (BFork (BLeaf 'A') (BFork (BLeaf 'C') (BLeaf 'B'))) (BLeaf 'D')), length (trees [0..4]) == 1680]


bases = "AGCT"
-- Problem 10, insertions

insertions :: [Char] -> [[Char]]
insertions twoBases = concatMap (insertionsHelper twoBases) bases --use ConcatMap to work through every base given

{-
  make required base combinations, one base at a time
-}
insertionsHelper :: [Char] -> Char -> [[Char]]
insertionsHelper [secondBase] insertBase = [insertBase:secondBase:[], secondBase:insertBase:[]]
insertionsHelper (firstBase:secondBase) insertBase = [insertBase:firstBase:secondBase] ++ (map (firstBase:) (insertionsHelper secondBase insertBase))

p10tests = [insertions "GC" == ["AGC","GAC","GCA","GGC","GGC","GCG","CGC","GCC","GCC","TGC","GTC","GCT"]]


-- Problem 11, deletions
deletions :: [a] -> [[a]]
deletions allBases = helper allBases 
         where helper [lastBase] = [[]]
               helper (firstBase:restOfBases) = [restOfBases] ++ (map (firstBase:) (helper restOfBases)) --form triplets and use map to recurse through

p11tests = [deletions "AGCT" == ["GCT","ACT","AGT","AGC"]]


-- Problem 12, substitutions
substitutions :: String -> [String]
substitutions threeBases = concatMap (subHelper threeBases) bases --use ConcatMap to work through every base given

{-
  substitute one base into given combination, one base at a time
-}
subHelper [lastBase] subBase = [[subBase]]
subHelper (firstBase:restOfBases) subBase = [subBase:restOfBases] ++ (map (firstBase:)(subHelper restOfBases subBase)) 

p12tests = [substitutions "ACT" == ["ACT","AAT","ACA","GCT","AGT","ACG","CCT","ACT","ACC","TCT","ATT","ACT"]]


-- Problem 13, transpositions
transpositions :: [a] -> [[a]]
transpositions allBases = helper allBases --interchange base one and two, base two and three, base three and four using map and recursion
    where helper (firstBase:secondBase:[])= [secondBase:firstBase:[]]
          helper (firstBase:secondBase:restOfBases) = [secondBase:firstBase:restOfBases] ++ (map (firstBase: ) (helper (secondBase:restOfBases)))

p13tests = [transpositions "GATC" == ["AGTC","GTAC","GACT"]]



tests = [p1tests,p2tests,p3tests,p4tests,p5tests,p6tests,p7tests,p8tests,p9tests,p10tests,p11tests,p12tests,p13tests]
likelyCorrect = let results = [and t | t <- tests] in (and results, filter (not.snd) $ zip [1..] results)