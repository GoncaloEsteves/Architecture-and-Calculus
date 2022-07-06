module Problem2 where

import DurationMonad
import Problem1

{-
   1. *Labelled* graph structure: nodes and labelled adjacency matrix
      (i.e. the labelled edges of the graph)
-}

adjT :: (Node,Node) -> Maybe Int
adjT p = case p of
            (A,B) -> Just 2
            (A,C) -> Just 3
            (A,F) -> Just 6
            (B,A) -> Just 30
            (B,C) -> Just 0
            (B,E) -> Just 4
            (B,F) -> Just 3
            (C,A) -> Just 60
            (C,B) -> Just 3
            (C,D) -> Just 50
            (D,C) -> Just 2
            (D,E) -> Just 3
            (E,B) -> Just 1
            (E,D) -> Just 3
            (E,F) -> Just 2
            (F,A) -> Just 4
            (F,B) -> Just 5
            (F,E) -> Just 3
            (_,_) -> Nothing


-- 2. Auxiliary functions

{-
   Function that returns the Int stored in a Maybe Int.
   (In this particular case we can define it likes this because an edge
    will always have a positive value. Also, we are assured that the place where
    the function will be used will only apply it on existing edges)
-}
dist :: Maybe Int -> Int
dist Nothing  = -1
dist (Just n) = n

-- Function that converts something into a Duration
createDuration :: a -> Duration a
createDuration n = return n

{-
   Given a node n and a list of nodes ns the function returns the nodes
   in ns that can be reached from n in one step together with the time
   necessary to reach them.
-}
tadjacentNodes :: Node -> [Node] -> [Duration Node]
tadjacentNodes n ns = do ns' <- adjacentNodes n ns
                         return (Duration(dist(adjT(n, ns')), ns'))

-- Function that determines if a path is Hamiltonion
hamiltonianCost :: Duration Path -> Bool
hamiltonianCost (Duration (_, []))     = False
hamiltonianCost (Duration (_, (x:xs))) = (x == last xs) && noRep xs


-- 3. Main body

{-
   For each node a in ns, if a is not already in p the function creates
   a new path (like in the previous problem) and computes its cost.
-}
taddToEnd :: Duration Path -> [Duration Node] -> [Duration Path]
taddToEnd p ns = do ns'   <- filter (\x -> not(elem (getValue x) (getValue p))) ns
                    paths <- [Duration ((getDuration p) + (getDuration ns'), choice(getValue p, [getValue ns']))]
                    return paths

{-
  Function that returns all Hamiltonian cycles starting from a given path
  and using a adjacent node up next
-}
hCyclesCostAux :: Duration Node -> Duration Path -> Int -> [Duration Path]
hCyclesCostAux n ns 0 = do adjn  <- tadjacentNodes (getValue n) allNodes
                           pths1 <- [Duration ((getDuration ns) + (getDuration adjn), choice((getValue ns), [(getValue adjn)]))]
                           pths2 <- filter (\x -> hamiltonianCost x) [pths1]
                           return pths2
hCyclesCostAux n ns x = do adjn  <- tadjacentNodes (getValue n) allNodes
                           pths1 <- taddToEnd ns [adjn]
                           pths2 <- hCyclesCostAux adjn pths1 (x-1)
                           return pths2

-- Computes all Hamiltonian cycles starting from a given node
hCyclesCost :: Node -> [Duration Path]
hCyclesCost n = do adjn  <- tadjacentNodes n allNodes
                   pths1 <- taddToEnd (createDuration [n]) [adjn]
                   pths2 <- hCyclesCostAux adjn pths1 ((length allNodes)-2)
                   return pths2

-- The main program
tsp = minimum . hCyclesCost