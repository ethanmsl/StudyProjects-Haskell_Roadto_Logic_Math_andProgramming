

--The Haskell Road to Logic, Maths(,) and Programming 2nd ed
--Chapter 01

-- return smallest integer in a list of integers; or error for empty list
mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)


-- appears to be an alt `min` function, just for the sake of manually defining it
min' :: Int -> Int -> Int
min' x y 
    | x <= y        = x
    | otherwise     = y


-- manually defiend version of `max`
max' :: Int -> Int -> Int 
max' x y
    | x >= y        = x
    | otherwise     = y


-- ex 1.9
mxmInt :: [Int] -> Int
mxmInt []       = error  "empty list"
mxmInt [x]      = x
mxmInt (x:xs)   = max x (mxmInt xs)

-- ex 1.10

removeFst :: Eq t => t -> [t] -> [t]
removeFst _ []      = []
removeFst a (x:xs)
    | a == x        = xs
    | otherwise     = x : removeFst a xs


srtInts :: [Int] -> [Int]
srtInts []      = []
srtInts xs       = m : (srtInts (removeFst m xs)) 
    where m = mnmInt xs

srtInts' :: [Int] -> [Int]
srtInts' []     = []
srtInts' xs     = let
                    m = mnmInt xs
                    in m : (srtInts' (removeFst m xs))


average :: [Int] -> Rational
average []      = error " empty list"
average xs      = toRational (sum xs) / toRational (length xs)

sum' :: [Int] -> Int
sum' []         = 0
sum' (x:xs)     = x + sum' xs

length' :: [a] -> Int
length' []      = 0
length' (x:xs)  = 1 + length' xs

-- 1.13
count :: Char -> [Char] -> Int
count _ []          = 0
count c (x:xs)
    | x == c        = 1 + count c xs
    | otherwise     = 0 + count c xs

-- 1.14
blowup :: String -> String
blowup []       = []
blowup xs   = blowup_hid_var 1 xs
-- easy to do this with an accessory function that basically counts .. but I feel like there should be a "pretty" way to do it.,..


repeatChar_blowup :: Int -> Char -> [Char]
repeatChar_blowup 0 _       = []
repeatChar_blowup n c       = c : repeatChar_blowup (n-1) c

-- lol, lol, lol -- looked up formal answer after not being sure hot to use the above without creating another internal function
-- that would pass a number ... and that's how they did it
-- ... part of me is def dissapointed, expected there was some elegant solution I didn't see

blowup_hid_var :: Int -> [Char] -> [Char]
blowup_hid_var _ []         = []
blowup_hid_var n (x:xs)     = (repeatChar_blowup n x) ++ blowup_hid_var (n+1) xs



-- 1.15
-- nicest would be to generalize ordering and indeed they kinda let us... well just take advantage of the fac thtat that's done
mnm :: Ord a => [a] -> a
mnm [] = error "empty list"
mnm [x] = x
mnm (x:xs) = min x (mnm xs)

srt :: Ord a => [a] -> [a]
srt [] = []
srt xs = m : (srt (removeFst m xs)) where m = mnm xs

prefix :: String -> String -> Bool
prefix [] ys            = True
prefix xs []            = False
prefix (x:xs) (y:ys)    = (x==y) && prefix xs ys

-- ex 1.17
substring :: String -> String -> Bool
substring [] _             = True
substring _ []             = False
substring sub (x:xs)       = prefix sub (x:xs) || substring sub xs


--map' :: (a->b) -> [a] -> [b]
--map' f []       = []
--map' f (x:xs)   = f x : map f xs

-- ex 1.20
lengths :: [[a]] -> [Int]
lengths []      = []
--lengths (x:xs)  = length' x : lengths xs
lengths x       = map length' x

-- ex 1.21
sumLengths :: [[a]] -> Int
sumLengths []       = 0
sumLengths xs       = sum' (lengths xs)