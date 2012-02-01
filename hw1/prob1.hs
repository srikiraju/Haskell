import Data.List

data Node = Node Int Float Float [Int]

--Checks if Dag. Assumes particular Node format
isDag :: [Node] -> Bool
isDag [] = error "Empty graph"
isDag [x] = True
isDag graph = foldl (checkDag graph) True graph


--Pull out the Node data struct from the graph
getNode :: [Node] -> Int -> Node
getNode [] id = error "Empty graph in getNode"
getNode [(Node x y1 y2 y3)] id
    | x == id = (Node x y1 y2 y3)
    | otherwise = error "Can't find id"
getNode ((Node x y1 y2 y3):xs) id
    | x == id = (Node x y1 y2 y3)
    | otherwise = getNode xs id


--Helper of CheckCycles
checkIntDag :: [Node] -> Node -> [Int] -> Bool -> Node -> Bool
checkIntDag _ _ _ False _ = False
checkIntDag graph source@(Node source_id _ _ _) visited _ int@(Node int_id _ _ adjacents) = if ( ( int_id == source_id ) || ( int_id `elem` visited ) )
    then False
    else let new_visited = visited++[int_id]
    in foldl (checkIntDag graph source new_visited) True (map (getNode graph) adjacents)

--Check if there is a cycle from all paths starting from this given node
checkDag :: [Node] ->  Bool -> Node -> Bool
checkDag _ False _ = False 
checkDag graph _ boo@(Node id _ _ adjacents) = foldl (checkIntDag graph boo [id]) True (map (getNode graph) adjacents)

--Is graph a Tree?
isTree :: [Node] -> Bool
isTree graph = let visiteds = map (checkTree graph []) graph
    in foldl (\istree visited -> if length (nub visited) == length visited then istree else False) True visiteds

--Check if there is a cycle from all paths starting from this given node
checkTree :: [Node] -> [Int] -> Node -> [Int]
checkTree graph visited boo@(Node id _ _ adjacents) = let new_visited = visited++[id]
    in foldl (checkTree graph) new_visited (map (getNode graph) adjacents)


main :: IO()
main = putStrLn(show (isTree( [(Node 1 1.0 1.0 [2, 3]),(Node 2 1.0 1.0 [4]),(Node 3 1.0 1.0 [4]),(Node 4 1.0 1.0 [])] )))


