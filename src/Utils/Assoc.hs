module Utils.Assoc
where

import Data.Maybe (fromMaybe)


type ASSOC a b = [(a,b)]


aLookup :: Eq a => [(a,b)] -> a -> b -> b
aLookup aList val defval = fromMaybe defval $ lookup val aList

aDomain :: ASSOC a b -> [a]
aDomain = map fst

aRange :: ASSOC a b -> [b]
aRange = map snd

aEmpty = []

