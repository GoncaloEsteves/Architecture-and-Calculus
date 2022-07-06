module Problem1 where

-- 1. Graph structure: nodes and adjacency matrix (i.e. the edges)

data Node = A | B | C | D | E | F deriving (Show,Eq,Ord)

adj :: (Node,Node) -> Bool
adj p = case p of
    (A,B) -> True
    (A,C) -> True
    (A,F) -> True 
    (B,A) -> True
    (B,C) -> True
    (B,E) -> True
    (B,F) -> True
    (C,A) -> True
    (C,B) -> True
    (C,D) -> True
    (D,C) -> True
    (D,E) -> True
    (E,B) -> True
    (E,D) -> True
    (E,F) -> True
    (F,A) -> True
    (F,B) -> True
    (F,E) -> True
    (_,_) -> False

type Path = [Node]


-- 2. Auxiliary functions

adjacentNodes :: Node -> [Node] -> [Node]
adjacentNodes n ns = filter (\x -> adj(n,x)) ns

allNodes :: [Node]
allNodes = [A,B,C,D,E,F]

choice :: ([a],[a]) -> [a]
choice = uncurry (++)

-- Function that determines if a list as no repeated elements
noRep :: Eq a => [a] -> Bool
noRep []     = True
noRep (x:xs) = (not (elem x xs)) && noRep xs

-- Function that determines if a path is Hamiltonion
hamiltonian :: Path -> Bool
hamiltonian []     = False
hamiltonian (x:xs) = (x == last xs) && noRep xs


-- 3. Main body

{- 
  For each node a in ns, if a is not already in p the function
  creates a new path by adding to the end of p the element a.
-}
addtoEnd :: Path -> [Node] -> [Path]
addtoEnd p ns =  do ns' <- filter (\a -> not(elem a p)) ns
                    path <- [choice(p, [ns'])]
                    return path

{-
  Function that returns all Hamiltonian cycles starting from a given path
  and using a adjacent node up next
-}
hCyclesAux :: Node -> Path -> Int -> [Path]
hCyclesAux n ns 0 = do adjn  <- adjacentNodes n allNodes
                       pths1 <- [choice(ns, [adjn])]
                       pths2 <- filter (\x -> hamiltonian x) [pths1]
                       return pths2
hCyclesAux n ns x = do adjn  <- adjacentNodes n allNodes
                       pths1 <- addtoEnd ns [adjn]
                       pths2 <- hCyclesAux adjn pths1 (x-1)
                       return pths2

-- Computes all Hamiltonian cycles starting from a given node
hCycles :: Node -> [Path]
hCycles n = do adjn  <- adjacentNodes n allNodes
               pths1 <- addtoEnd [n] [adjn]
               pths2 <- hCyclesAux adjn pths1 ((length allNodes)-2)
               return pths2