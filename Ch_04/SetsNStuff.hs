module STAL

where

import Data.List
--import DB


naturals = [0..]
evens1  = [n | n <- naturals, even n]
odds1   = [n | n <- naturals, odd n]