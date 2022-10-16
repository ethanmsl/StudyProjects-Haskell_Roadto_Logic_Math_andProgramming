

-- recap of earlier primes programs

prime :: Integer -> Bool
prime n | n < 1         = error "not a positive integer"
        | n == 1        = False
        | otherwise     = ldp n == n 
        where
            ldp             = ldpf primes
            ldpf (p:ps) m   | rem m p == 0      = p
                            | p^2 > m           = m
                            | otherwise         = ldpf ps m
            primes = 2 : filter prime [3..]


-----------------------------------------------------------
-- Eratosthenes's Sieve
-- works by marking multiples of existing primes as 0 and ignoring 0s
sieve :: [Integer] -> [Integer]
sieve (0 : xs)      = sieve xs
sieve (n : xs)      = n : sieve (mark xs 1 n)
    where
        mark :: [Integer] -> Integer -> Integer -> [Integer]
        mark (y:ys) k m     | k == m        = 0 : (mark ys  1    m)
                            | otherwise     = y : (mark ys (k+1) m)

primes_erat :: [Integer]
primes_erat = sieve [2..]


--------------------
-- proper divisors of a number (i.e. all the positive integers, not including the number itself .. so 1 counts, but n itself doesn't -\_0_/-)
pdivisors :: Integer -> [Integer]
pdivisors n = [d | d <- [1.. (n-1)], rem n d == 0]

-- "Perfect Number" -- number (post integer) whose proper divisors sum to the number itself
isPerfectN :: Integer -> Bool
isPerfectN n        = sum (pdivisors n) == n


-- Prime Pairs
primePairs :: [(Integer, Integer)]
primePairs      = pairs primes_erat
    where
    pairs (x : y : xys)     | x+2 == y      = (x,y) : pairs (y:xys)
                            | otherwise     = pairs (y:xys)

-- Prime Triples !!There only exists 1 PrimeTriple ...~lol!!
--primeTriples :: [(Integer, Integer, Integer)]
--primeTriples    = overlap primePairs
--    where
--    overlap (pair_1 : pair_2 : ps)      | snd pair_1 == fst pair_2     = (fst pair_1, snd pair_1, snd pair_2) : overlap (pair_2:ps)
--                                        | otherwise                    = overlap (pair_2:ps)