 
 -- data Bool = True | False

 -- not :: Bool -> Bool

 -- (&&) :: Bool -> Bool -> Bool

 -- (||) :: Bool -> Bool -> Bool

infix 1 ==>
(==>) :: Bool -> Bool -> Bool
x ==> y     = not x || y

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y     = x == y

infixr 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y     = x /= y

--------------------------------------

p :: Bool
p = True
q :: Bool
q = False

formula1 :: Bool
formula1 = (not p) && (p ==> q) <=> not (q && (not p))


formula2 :: Bool -> Bool -> Bool
formula2 p q = ((not p) && (p ==> q) <=> not (q && (not p)))

valid1 :: (Bool -> Bool) -> Bool
valid1 bf =  (bf True) && (bf False)

excluded_middle :: Bool -> Bool
excluded_middle p = p || not p

valid2 :: (Bool->Bool->Bool)->Bool
valid2 bf =    (bf True True)
            && (bf True False)
            && (bf False True)
            && (bf False False)

form1 :: Bool -> Bool -> Bool
form1 p q =   p ==> (q==>p)
form2 :: Bool -> Bool -> Bool
form2 p q =   (p ==> q) ==> p

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [bf p q r | p <- [True, False],
                            q <- [True, False],
                            r <- [True, False]]


valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [bf p q r s | p <- [True, False],
                              q <- [True, False],
                              r <- [True, False],
                              s <- [True, False]]


---------------------------------------------------
moreN3 = \x -> x>3
moreN4 x = x>4
digies = [1,2,3,4,5,6]

--map moreN3 digies
--any moreN3 digies
--all moreN3 digies

--every digies moreN3  <-- must not be generally available
--some digies moreN3   <-- must not be generally available


-- ex 2.51  -- give true if precisely one member of list produces a True
unique :: (a -> Bool) -> [a] -> Bool
unique f xs     = length (filter f xs) == 1

-- ex 2.52  -- True if number of vals in a Boolean list that are T is even
parityEven :: [Bool] -> Bool
parityEven xs  = length (filter (\x->x==True) xs) `mod` 2 == 0

-- ex 2.53  -- if even number of applied val->Bool's yields a list with even number of Ts
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR f xs     = parityEven (map f xs)
