-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 9
--
-- Week 10(18-22 Nov.)
module Tutorial10 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)

type Row a     =  [a]
type Col a     =  [a]
type Matrix a  =  Col (Row a)
type Digit     =  Char

digits :: [Digit]
digits =  ['1'..'9']

blank :: Digit -> Bool
blank d  =  d == ' '

-- 2.
groups :: [a] -> [[a]]
groups = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy 0 xs = [xs]
groupBy n [] = []
groupBy n xs 
    |mod (length xs) n == 0 = [take n xs] ++ (groupBy n (drop n xs))
    |otherwise = error"Not Divisible"

-- 3.
intersperse :: a -> [a] -> [a]
intersperse x xs = concat [[x] ++ [y] | y <- xs] ++ [x]

-- 4.
showRow :: String -> String
showRow xs =  concat (intersperse "|" (groups xs))

-- 5.
showGrid :: Matrix Digit -> [String]
showGrid xs =intersperse "-------------"  (concat (groups xs))

-- 6.
put2 xs= put (showGrid [showRow x | x <- xs])
put :: Matrix Digit -> IO ()
put [] = putStr []
put (x:xs) = putStr x >>putChar '\n' >> put xs

-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices [] = []
choices (x:xs)= [if y == ' ' then "123456789" else [y]| y <- x ]: choices xs

-- 8.
cp :: [[a]] -> [[a]]
cp []        =  [[]]
cp (xs:xss)  =  [ x:ys | x <- xs, ys <- cp xss ]
expand2 :: Matrix [Digit] -> [Matrix Digit]
expand2 = cp . map cp
expand :: Matrix [Digit] -> [Matrix Digit]
expand xs= cp [cp x | x <-xs]
lengthexp xs = product [product [fromIntegral (length x) | x <- a] | a <- xs]
length2 xs =  (lengthexp xs)

-- 11, 12, 13.
-- transpose :: [[a]] -> [[a]]
-- transpose [xs]      =  [[x] | x <- xs]
-- transpose (xs:xss)  =  zipWith (:) xs (transpose xss)

ungroup :: [[a]] -> [a]
ungroup [] = []
ungroup (x:xs) = x ++ ungroup xs

rows, cols, boxs :: Matrix a -> Matrix a
rows  xs=  transpose (transpose xs)
cols  xs= transpose xs
boxs xs =  (map ungroup  ((ungroup x)))
    where
    x = map cols (groups  ((map groups xs)))

-- 14.
distinct :: Eq a => [a] -> Bool
distinct  x = nub x == x

-- 15.
valid :: Matrix Digit -> Bool
valid  xs = and [elem x y | x <- "123456789", y <- xs] && and [ elem x y | x <- "123456789" , y <- cols xs] && and [elem x y | x <- "123456789" , y <- boxs xs]
valid2 xs = and [distinct x | x <-xs] && and [distinct x | x <- cols xs] && and [ distinct x | x <- boxs xs]

-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple =  filter valid2 . expand . choices

-- 18.
the :: [Digit] -> Digit
the [d]  =  d
single :: [Digit] -> Bool
single [d]  =  True
single _    =  False
pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row  =  [ ds \\ fixed rest | (ds,rest) <- splits row ]
listacasos row =concat [x | x <-row, length x == 1]
splits :: [a] -> [(a, [a])]
splits []      =  []
splits (x:xs)  =  (x,xs) : [ (y,x:ys) | (y,ys) <- splits xs ]
fixed :: Row [Digit] -> [Digit]
fixed row  =  [ the ds | ds <- row, single ds ]
-- 19.
pruneBy :: (Matrix [Digit] -> Matrix [Digit])
             -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy rows . pruneBy cols . pruneBy boxs

--close
close :: (Eq a, Ord a) => [(a,a)] -> [(a,a)]
close pairs = nub (sort (pairs ++ [ (x,z) | (x,y) <- pairs, (y',z) <- pairs, y == y' ]))

-- 20.
many :: Eq a => (a -> a) -> a -> a
many f x
        | x == f x = x
        | otherwise = many f (f x)

-- 21.
extract :: Matrix [Digit] -> Matrix Digit
extract xs= [[the [x | x <-a]| a <- b]| b <-xs]
        where
        the [x] = x

-- 22.
solve :: Matrix Digit -> Matrix Digit
solve xs 
    |valid  (extract (many (prune) (choices xs))) = extract (many (prune) (choices xs))
    |otherwise = error "Not solution"

-- 23.
failed :: Matrix [Digit] -> Bool
failed mat  =  or[or[x == ""| x <- a]| a <- mat] 

-- 24.
solved :: Matrix [Digit] -> Bool
solved =  all (all single)

-- 25.

break2 :: (a -> Bool)->[a]-> [a] -> ([a],[a])
break2 f [] a = (a,[])
break2 f (x:xs) a
                | f x == True = break2 f (xs) (a ++[x])
                | f x == False = ((a),x:xs)
braking f xs = break2 f xs []
-- 27.
counts :: Matrix [Digit] -> [Int]
counts =  filter (> 1) . map length . concat
shortest :: Matrix [Digit] -> Int
shortest xs = minimum (counts xs)

expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = [preMat ++ [preRow ++ [[d]] ++ postRow]++postMat| d <-ds]
    where
    short ds = length ds == shortest mat
    (preMat, row:postMat)  =  break (any short) mat
    (preRow, ds:postRow)   =  break short row
-- 28.
search :: Matrix Digit -> [Matrix Digit]
search =  loop . choices
  where 
  loop mat | solved pruned  =  [extract pruned]
           | failed pruned  =  []
           | otherwise      =  concat (map loop (expand1 pruned))
           where pruned = many prune mat
shows2 [] = putStr []
shows2 (x:xs) = put2 x >> putChar '\n' >> shows2 xs
-- Example from Bird

book    :: Matrix Digit
book    =  ["  4  57  ",
            "     94  ",
            "36      8",
            "72  6    ",
            "   4 2   ",
            "    8  93",
            "4      56",
            "  53     ",
            "  61  9  "]

-- Examples from websudoku.com
byBox :: Matrix Digit
byBox = ["123123123","456456456","789789789","123123123","456456456", "789789789","123123123", "456456456","789789789"]
easy    :: Matrix Digit
easy    =  ["    345  ",
            "  89   3 ",
            "3    2789",
            "2 4  6815",
            "    4    ",
            "8765  4 2",
            "7523    6",
            " 1   79  ",
            "  942    "]

medium  :: Matrix Digit
medium  =  ["   4 6 9 ",
            "     3  5",
            "45     86",
            "6 2 74  1",
            "    9    ",
            "9  56 7 8",
            "71     64",
            "3  6     ",
            " 6 9 2   "]

hard    :: Matrix Digit
hard    =  ["9 3  42  ",
            "4 65     ",
            "  28     ",
            "     5  4",
            " 67 4 92 ",
            "1  9     ",
            "     87  ",
            "     94 3",
            "  83  6 1"]

evil    :: Matrix Digit
evil    =  ["  9      ",
            "384   5  ",
            "    4 3  ",
            "   1  27 ",
            "2  3 4  5",
            " 48  6   ",
            "  6 1    ",
            "  7   629",
            "     5   "]
br :: IO ()
br = putStrLn "***"

puts :: [Matrix Digit] -> IO ()
puts  =  sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g  =  put g >>
             puts (search g) >>
             br
       
main =  puzzle easy >>
        puzzle medium >>
        puzzle hard >>
        puzzle evil

