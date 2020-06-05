module NinjaInfo where 

import Data.List
import Data.Char
import Data.Ord


data Ninja = Ninja{name:: String, country:: Char,score:: Float, r:: Int} deriving Show

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

data Country = Country{name :: String, ninjas :: [Ninja], code :: Char, promoted :: Bool} deriving Show
c1 = Country{name="fire", ninjas = fire, code='f', promoted= False}
c2 = Country{name="water", ninjas = lightning ,code='w', promoted= False}
c3 = Country{name="earth", ninjas= water,  code='e', promoted= False}

--instance Ord Ninja where
  --compare a b = if (r a) == (r b) then compare (score b) (score a) else compare (r a) (r b)


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


getNinjasByCountry :: Char -> [Ninja]
getNinjasByCountry 'f' = qSort fire
getNinjasByCountry 'l' = qSort lightning
getNinjasByCountry 'w' = qSort water

getAvailableNinjas :: [Ninja]
getAvailableNinjas = (fire ++ lightning ++ water)

getNinjas :: [Ninja]
getNinjas = qSort getAvailableNinjas


insertNinja :: Ninja -> Ninja
insertNinja x = x -- compare function for ninjas


sortNinjas :: [Ninja] -> [Ninja]
sortNinjas  []     = []
sortNinjas [x] = [insertNinja x]
sortNinjas(x:xs) = insertNinja x : sortNinjas xs

-- call qSort with the selected country, print the list 
-- give wwarning if country has already promoted ninja

-- main = print(sortNinjas fire)