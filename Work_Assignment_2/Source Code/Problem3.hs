module Problem3 where

import qualified Data.Text
import qualified Data.Text.IO
import Data.List
import DurationMonad

-- 1. Graph structure: nodes and adjacency matrix (i.e. the edges) 

data Node = A | B | C | D | E | F deriving (Show,Eq,Ord)

type Path   = [Node]
type Graph  = [(Node, Node)]
type WGraph = [((Node, Node), Int)]

initialgraph :: Graph
initialgraph =
    [(A,B), (A,C), (A,F),
     (B,A), (B,C), (B,E), (B,F),
     (C,A), (C,B), (C,D),
     (D,C), (D,E),
     (E,B), (E,D), (E,F),
     (F,A), (F,B), (F,E)]

initialWgraph :: WGraph
initialWgraph =
    [((A,B), 2) , ((A,C), 3), ((A,F), 6),
     ((B,A), 30), ((B,C), 0), ((B,E), 4), ((B,F), 3),
     ((C,A), 60), ((C,B), 3), ((C,D), 50),
     ((D,C), 2) , ((D,E), 3),
     ((E,B), 1) , ((E,D), 3), ((E,F), 2),
     ((F,A), 4) , ((F,B), 5), ((F,E), 3)]


-- 2. Auxiliary functions

{------------------------------------------------------GENERAL AUXILIAR FUNCTIONS-------------------------------------------------------}

allNodes :: [Node]
allNodes = [A,B,C,D,E,F]

choice :: ([a],[a]) -> [a]
choice = uncurry (++)

-- Function that determines if a list as no repeated elements
noRep :: Eq a => [a] -> Bool
noRep []     = True
noRep (x:xs) = (not (elem x xs)) && noRep xs

-- Function that converts a String to its respective Node
wordToNode :: String -> Node
wordToNode "A" = A
wordToNode "B" = B
wordToNode "C" = C
wordToNode "D" = D
wordToNode "E" = E
wordToNode "F" = F

{-------------------------------------------------AUXILIAR FUNCTIONS FOR NORMAL GRAPHS--------------------------------------------------}

adj :: (Node,Node) -> Graph -> Bool
adj p g = elem p g

adjacentNodes :: Node -> [Node] -> Graph -> [Node]
adjacentNodes n ns g = filter (\x -> adj (n,x) g) ns

-- Function that determines if a path is Hamiltonion
hamiltonian :: Path -> Bool
hamiltonian []     = False
hamiltonian (x:xs) = (x == last xs) && noRep xs

-- Function that converts a List with String to a pair with the two first Nodes
getNodes :: [String] -> (Node, Node)
getNodes (x:y:xs) = (wordToNode x,wordToNode y)

-- Function that converts a String to a Pair of Nodes
stringToNode :: String -> (Node, Node)
stringToNode = getNodes . words

-- Function that converts a list of Strings to a Graph
linesToGraph :: [String] -> Graph
linesToGraph ss = map (\x -> stringToNode x) ss

{------------------------------------------------AUXILIAR FUNCTIONS FOR WEIGHTED GRAPHS-------------------------------------------------}

adjT :: (Node,Node) -> WGraph -> Bool
adjT _ []                      = False
adjT (x, y) (((a, b), _) : xs) = if (x == a && y == b) then True
                                 else adjT (x, y) xs

{-
  Function that returns the distance between two nodes connected by an edge.
  We can define it like this because we garantee that it will be used in correct cases.
-}
getDist :: (Node, Node) -> WGraph -> Int
getDist _ []           = 0
getDist p ((p', n):ps) = if (p == p') then n
                        else getDist p ps

{-
  Given a node n and a list of nodes ns the function returns the nodes
  in ns that can be reached from n in one step together with the time
  necessary to reach them.
-}
tadjacentNodes :: Node -> [Node] -> WGraph -> [Duration Node]
tadjacentNodes n ns g = do ns' <- filter (\x -> adjT (n, x) g) ns
                           return (Duration (getDist (n, ns') g, ns'))

-- Function that converts something into a Duration
createDuration :: a -> Duration a
createDuration n = return n

-- Function that determines if a path is Hamiltonion
hamiltonianCost :: Duration Path -> Bool
hamiltonianCost (Duration (_, []))     = False
hamiltonianCost (Duration (_, (x:xs))) = (x == last xs) && noRep xs

-- Function that converts a List with String to a pair with the two first Nodes
getWNodes :: [String] -> ((Node, Node), Int)
getWNodes (x:y:z:xs) = ((wordToNode x, wordToNode y), read z :: Int)

-- Function that converts a String to a Pair of Nodes and Distance
stringToWNode :: String -> ((Node, Node), Int)
stringToWNode = getWNodes . words

-- Function that converts a list of Strings to a WGraph
linesToWGraph :: [String] -> WGraph
linesToWGraph ss = map (\x -> stringToWNode x) ss


-- 3. Main body

{---------------------------------------------------MAIN FUNCTIONS FOR NORMAL GRAPHS----------------------------------------------------}

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
hCyclesAux :: Node -> [Node] -> Path -> Graph -> Int -> [Path]
hCyclesAux n an ns g 0 = do adjn  <- adjacentNodes n an g
                            pths1 <- [choice(ns, [adjn])]
                            pths2 <- filter (\x -> hamiltonian x) [pths1]
                            return pths2
hCyclesAux n an ns g x = do adjn  <- adjacentNodes n an g
                            pths1 <- addtoEnd ns [adjn]
                            pths2 <- hCyclesAux adjn an pths1 g (x-1)
                            return pths2

-- Computes all Hamiltonian cycles starting from a given node
hCycles :: Node  -> [Node] -> Graph -> [Path]
hCycles n an g = do adjn  <- adjacentNodes n an g
                    pths1 <- addtoEnd [n] [adjn]
                    pths2 <- hCyclesAux adjn an pths1 g ((length an)-2)
                    return pths2

-- Function that reads a file and creates a Graph
readGraphFromFile :: String -> IO Graph
readGraphFromFile filename = do let graph = []
                                file <- Data.Text.IO.readFile filename
                                let filelines = lines (Data.Text.unpack file)
                                    graph = linesToGraph filelines
                                return graph

-- Computes all Hamiltonian cycles starting from a given Node
hCyclesFromFile :: Node -> [Node] -> String -> IO [Path]
hCyclesFromFile n an f = do g <- readGraphFromFile f
                            let p = hCycles n an g
                              in return p

-- Function that determines all paths between two nodes using only some given nodes
pathsBetween :: Node -> Node -> [Node] -> Graph -> [Path]
pathsBetween n1 n2 ns g = if (n1 == n2) then [[n1]]
                          else do adj   <- adjacentNodes n1 ns g
                                  pths1 <- pathsBetween adj n2 (delete n1 ns) g
                                  pths2 <- [(n1:pths1)]
                                  return pths2

{--------------------------------------------------MAIN FUNCTIONS FOR WEIGHTED GRAPHS---------------------------------------------------}

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
hCyclesCostAux :: Duration Node -> [Node] -> Duration Path -> WGraph -> Int -> [Duration Path]
hCyclesCostAux n an ns g 0 = do adjn  <- tadjacentNodes (getValue n) an g
                                pths1 <- [Duration ((getDuration ns) + (getDuration adjn), choice((getValue ns), [(getValue adjn)]))]
                                pths2 <- filter (\x -> hamiltonianCost x) [pths1]
                                return pths2
hCyclesCostAux n an ns g x = do adjn  <- tadjacentNodes (getValue n) an g
                                pths1 <- taddToEnd ns [adjn]
                                pths2 <- hCyclesCostAux adjn an pths1 g (x-1)
                                return pths2

-- Computes all Hamiltonian cycles starting from a given node
hCyclesCost :: Node  -> [Node] -> WGraph -> [Duration Path]
hCyclesCost n an g = do adjn  <- tadjacentNodes n an g
                        pths1 <- taddToEnd (createDuration [n]) [adjn]
                        pths2 <- hCyclesCostAux adjn an pths1 g ((length an)-2)
                        return pths2

-- The main program
tsp :: Node -> [Node] -> WGraph -> Duration Path
tsp n an g = minimum (hCyclesCost n an g)

-- Function that reads a file and creates a WGraph
readWGraphFromFile :: String -> IO WGraph
readWGraphFromFile filename = do let graph = []
                                 file <- Data.Text.IO.readFile filename
                                 let filelines = lines (Data.Text.unpack file)
                                     graph = linesToWGraph filelines
                                 return graph

-- Computes all Hamiltonian cycles starting from a given Node
hCyclesCostFromFile :: Node -> [Node] -> String -> IO [Duration Path]
hCyclesCostFromFile n an f = do g <- readWGraphFromFile f
                                let p = hCyclesCost n an g
                                  in return p

-- Function that determines all paths between two nodes using only some given nodes
pathsBetweenW :: Node -> Node -> [Node] -> WGraph -> [Duration Path]
pathsBetweenW n1 n2 ns g = if (n1 == n2) then [createDuration [n1]]
                           else do adj   <- tadjacentNodes n1 ns g
                                   pths1 <- pathsBetweenW (getValue adj) n2 (delete n1 ns) g
                                   pths2 <- [(Duration (((getDuration adj) + (getDuration pths1)), (n1:(getValue pths1))))]
                                   return pths2

-- Function that determines the shortest path between two nodes using only some given nodes
shortestPath :: Node -> Node -> [Node] -> WGraph -> Duration Path
shortestPath n1 n2 an g = minimum (pathsBetweenW n1 n2 an g)

-- Function that determines all the shortest paths between a node and all of the nodes in a list
apspAux2 :: Node -> [Node] -> [Node] -> WGraph -> [Duration Path]
apspAux2 n ns an g = do ns'  <- delete n ns
                        let pths = shortestPath n ns' an g
                          in return pths

apspAux :: [Node] -> [Node] -> [Node] -> WGraph -> [Duration Path]
apspAux ns ns' an g = concat (map (\n -> apspAux2 n ns' an g) ns)

-- Function that determines all the shortest paths between all nodes in a list
apsp :: [Node] -> [Node] -> WGraph -> [Duration Path]
apsp n an g = apspAux n n an g