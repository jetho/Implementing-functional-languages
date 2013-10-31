module Utils.Heap where

import Utils.Assoc


-- size, available, allocated
type Heap a = (Int, [Int], [(Int, a)])
type Addr = Int
type NameSupply = Int


hInitial :: Heap a
hInitial = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), cts) n = (((size+1), free, (next, n):cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n = ( size, free, (a,n) : remove cts a )

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a = (size-1, a:free, remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (_, _, cts) a = aLookup cts a $ error msg
    where 
        msg = "Can't find node " ++ show a ++ " in heap"

hAddresses :: Heap a -> [Addr]
hAddresses (_, _, cts) = map fst cts 

hSize :: Heap a -> Int
hSize (size, _, _) = size

hNull :: Addr
hNull = 0

isHNull :: Addr -> Bool
isHNull = (== hNull)

remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] a = error ("Attempt to update or free nonexistant address #" ++ shownum a)
remove ((a',n):cts) a
   | a == a' = cts
   | a /= a' = (a', n) : remove cts a

shownum :: Int -> String
shownum = show

showAddr :: Addr -> String
showAddr = ("#" ++) . shownum

getName :: NameSupply -> [Char] -> (NameSupply, [Char])
getName name_supply prefix = (name_supply + 1, makeName prefix name_supply)

getNames :: NameSupply -> [[Char]] -> (NameSupply, [[Char]])
getNames name_supply prefixes = (name_supply + length prefixes, zipWith makeName prefixes [name_supply..])

makeName :: [Char] -> Int -> [Char]
makeName prefix ns = prefix ++ "_" ++ shownum ns

initialNameSupply :: NameSupply
initialNameSupply = 0

