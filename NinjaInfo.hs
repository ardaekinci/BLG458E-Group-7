module NinjaInfo where 

import Data.List
import Data.Char
import Data.Ord

data Ninja = Ninja{name:: String, country:: Char,score:: Float, r:: Int} deriving(Eq, Show)

first = Ninja{name="first", country='f', score=130.1, r=0}
sec = Ninja{name="sec", country='f', score=110.1, r=0}

b = Ninja{name="b", country='f', score=150.1, r=1}

c = Ninja{name="c", country='f', score=110.1, r=1}

s = Ninja{name="s", country='f', score=100.1, r=2}
third = Ninja{name="third", country='l', score=120.1, r=1}
fourth = Ninja{name="fourth", country='l', score=100.1, r=0}
fifth = Ninja{name="fifth", country='w', score=140.1, r=0}
sixth = Ninja{name="sixth", country='w', score=123.1, r=1}


fire :: [Ninja]
fire = [s,first,c,b,sec]

lightning :: [Ninja] 
lightning = [third,fourth]

water :: [Ninja] 
water = [fifth,sixth]

-- keep all countries list 
all_countries :: [Ninja]
all_countries = [s,first,c,b,sec,third,fourth,fifth,sixth]


--insertNinja :: Ninja -> [Ninja]
--insertNinja x -- insert each element

--sortNinjas :: [Ninja] -> [Ninja]
--sortNinjas  []     = []
--sortNinjas [x] = [insertNinja x]
--sortNinjas(x:xs) = insertNinja x : sortNinjas xs


--instance Ord Ninja where
  --compare a b = if (r a) == (r b) then compare (score b) (score a) else compare (r a) (r b)

-- write a special comparision for ninja first check round and then score**

compareNinja :: Ninja -> Ninja -> Bool
compareNinja n1 n2 
    | (r n1) > (r n2) = True
	| (r n1) == (r n2) && (score n1) < (score n2) = True
	| otherwise = False


qSort :: [Ninja] -> [Ninja]
qSort []     = []
qSort (x:xs) = qSort smaller ++ [x] ++ qSort larger
  where
    smaller = [a | a <- xs, compareNinja x a]
    larger  = [b | b <- xs, compareNinja b x]


-- call qSort with the selected country, print the list 
-- give wwarning if country has already promoted ninja

-- main = print(sortNinjas fire)