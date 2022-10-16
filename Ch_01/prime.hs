import Distribution.Compat.Lens (_1)
--The Haskell Road to Logic, Maths(,) and Programming 2nd ed
--Chapter 01



divides :: Integral a => a -> a -> Bool
divides denominator numerator = rem numerator denominator == 0
-- NOTE:  denominator -> numerator -> Bool
--        denom and num are flipped (orderwise) relative to `rem`


-- Least Divisor (`ld`) of numerator (`n`)
--ld :: Integral t => t -> t
ld :: Integral t => t -> t
ld n = ldf 2 n

-- Least Divisor  starting From (`ldf`) from_inclusive integer (`k`) of numerator (`n`)
ldf :: Integral t => t -> t -> t
ldf k n  
    | divides k n   = k
    | k^2 > n       = n             -- this is effectively an optimization step as ld can't be more than n_>root_>floor (yes, this is ceilng, but still)
    | otherwise     = ldf (k+1) n

-- I really don't like this set-upo
-- LDF looks like it shoudl eitehr give the least divisor >= k 
-- or at least only the least non-1 divisor
-- but as is ldf 1 15 ~~>1  while ldf 4 15 ~~> 15  .... not great.
-- ... I'm going to remove the middle guard
-- ... could be okay if I made a point that it was an internal-only function, tuned for performance specific to it's use in laid out functions


prime0 :: Integral a => a -> Bool
prime0 n
    | n < 1         = error "not a postive integer"
    | n == 1        = False
    | otherwise     = ld n == n


factors :: Integer -> [Integer]
factors n
    | n < 1         = error "argument not positive"
    | n == 1        = []
    | otherwise     = p : factors (div n p) where p = ld n


primes0 :: [Integer]
primes0 = filter prime0 [2..]

primes1 :: [Integer]
primes1 = 2: filter prime [3..]

-- least prime divisor
ldp :: Integer -> Integer
ldp = ldpf primes1

-- least (prime) divisor from (inclusive)...
ldpf :: [Integer]-> Integer -> Integer
--ldpf [] _1              = error "empty list provided instead of list of primes" --I'd like to tell this it can only be called in ldp as above... huh... as a way around... huh not sure how I feel about this
ldpf (p:ps) n
    | rem n p == 0      = p
    | p^2 > n           = n  -- this is effectively an optimization step as ld can't be more than n_>root_>floor (yes, this is ceilng, but still)
    | otherwise         = ldpf ps n

prime :: Integer -> Bool
prime n 
    | n < 1         = error "not a positive integer"
    | n == 1        = False
    | otherwise     = ldp n == n