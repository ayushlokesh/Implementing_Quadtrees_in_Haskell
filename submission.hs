--The Algebraic Datatype representing QuadTree
data QuadTree  = Black | White | Combine QuadTree QuadTree QuadTree QuadTree deriving(Eq, Show)

--Implementations for allBlack and allWhite functions
allBlack :: Int -> QuadTree
allBlack z = Black

allWhite :: Int -> QuadTree
allWhite z = White

--Implementations for clockwise and anticlockwise functions
clockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
clockwise Black Black Black Black = Black
clockwise White White White White = White
clockwise  q1 q2 q3 q4  = Combine q1 q2 q3 q4 

anticlockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
anticlockwise Black Black Black Black = Black
anticlockwise White White White White = White
anticlockwise  q1 q2 q3 q4  =  Combine q1 q4 q3 q2 

-- The function getListOfCoord takes in the QuadTree and a string and returns a list of the strings 
--(here each element of the list represents the coordinate of a node in the QuadTree)
-- For getting the list of coordinates of all the nodes of a given Quadtree 'q', we call getListOfCoords q "" .
getListOfCoord :: QuadTree -> String -> [String]
getListOfCoord Black s = [s]
getListOfCoord White s = [s]
getListOfCoord (Combine q1 q2 q3 q4) s =
  (getListOfCoord q1 (s ++ "1")) ++
  (getListOfCoord q2 (s ++ "2")) ++
  (getListOfCoord q3 (s ++ "3")) ++
  (getListOfCoord q4 (s ++ "4"))
  
-- The function getColor takes in the Parent QuadTree and a Coordinate of the node (as a string) we want the color of,
-- and returns a White or a Black
getColor :: QuadTree -> String -> QuadTree
getColor q [] = q
getColor (Combine q1 q2 q3 q4) ('1':s) = getColor q1 s
getColor (Combine q1 q2 q3 q4) ('2':s) = getColor q2 s
getColor (Combine q1 q2 q3 q4) ('3':s) = getColor q3 s
getColor (Combine q1 q2 q3 q4) ('4':s) = getColor q4 s

--The function checkSameColor takes two QuadTrees and returns a boolean value for if they are the same color.
checkSameColor :: QuadTree -> QuadTree -> Bool
checkSameColor q1 q2 = q1 == q2

--The checkNeighbour functions takes two coordinates of the two nodes (as string) that we want to check
--and returns a boolean value based on if they belong to the quadrants which are adjacent.
checkNeighbour :: String -> String -> Bool
checkNeighbour  [] [] = False                           --Base Case
checkNeighbour s "" = False                             --Base Case
checkNeighbour "" s = False                             --Base Case

checkNeighbour (x:s1) (y:s2) =                                         --Step Case
    if (x == y) then checkNeighbour s1 s2
    else if read [x] + read [y] == 3 || read [x] + read [y] == 7
        then if read [x] == 1 || read [x] == 4
                then checkSecNeb s1 s2 "h"
                else if read [x] == 2 || read [x] == 3
                        then checkSecNeb s2 s1 "h"
                        else False
        else if read [x] + read [y] == 5
                then if read [x] == 1 || read [x] == 2
                        then checkSecNeb s1 s2 "v"
                        else if read [x] == 4 || read [x] == 3
                                then checkSecNeb s2 s1 "v"
                                else False
                else False

--The function checkSecNeb takes two coordinates of the two nodes (as string) that we want to check and also takes in an
--indicator "h" or "v" to indicate if the the quadrants the nodes belong to are adjacent horizontally or vertically
--returns a boolean value based on if they are adjacent, this is used as a helper function for checkNeighbour.
checkSecNeb :: String -> String -> String -> Bool
checkSecNeb [] [] s = True                                                                      --Base Case


checkSecNeb (x:s1) [] "h" = (read [x]  == 2 || read [x] == 3) && checkSecNeb s1 [] "h"          --Step Case
checkSecNeb [] (x:s1) "h" = (read [x]  == 1 || read [x] == 4) && checkSecNeb [] s1 "h"          --Step Case


checkSecNeb (x:s1) [] "v" = (read [x]  == 3 || read [x] == 4) && checkSecNeb s1 [] "v"          --Step Case
checkSecNeb [] (x:s1) "v" = (read [x]  == 1 || read [x] == 2) && checkSecNeb [] s1 "v"          --Step Case

checkSecNeb (x:s1) (y:s2) "h" =  (read [x] == 2 || read [x] == 3) && ((read [x] + read [y]) == 3 || (read [x] + read [y]) == 7) && checkSecNeb s1 s2 "h"          --Step Case
checkSecNeb (x:s1) (y:s2) "v" = (read [x] == 3 || read [x] == 4) && (read [x] + read [y]) == 5 && checkSecNeb s1 s2 "v"                 --Step Case

checkSecNeb _ _ _ = False





--The add function takes a parent QuadTree, a node (that needs to be checked for flipping) and two lists of all nodes of the parent QuadTree
--and works out if it needs to be flipped based on the neighbours, by returning a Int value.
add :: QuadTree -> String -> [String] -> [String] -> QuadTree -> Int
add q1 s [] cl pq = 0
add q1 s (s1:sl) cl pq = if (checkNeighbour s s1 && (checkSameColor q1 (getColor pq s1)) ) 
                            then  (1 + (add q1 s sl cl pq))
                        else if (checkNeighbour s s1 && (not (checkSameColor q1 (getColor pq s1))) ) 
                            then (-1 + (add q1 s sl cl pq))
                         else add q1 s sl cl pq


--The function 'flip' flips the color of the node based on the value returned by add function. It takes in the node,
--list of nodes, and the parent QuadTree and returns the blurred QuadTree. It acts as a helper function for blur. 
flips :: QuadTree -> String -> [String] -> [String] -> QuadTree -> QuadTree
flips Black s sl cl pq = if (add Black s sl cl pq) < 0 then White
                        else Black
flips White s sl cl pq = if (add White s sl cl pq) < 0 then Black
                        else White
flips (Combine q1 q2 q3 q4) s sl cl pq = clockwise (flips q1 (s++"1") sl cl pq) (flips q2 (s++"2") sl cl pq) (flips q3 (s++"3") sl cl pq) (flips q4 (s++"4") sl cl pq)





--Implementation for blur Function
--It calls the helper function flips to blur the whole QuadTree
blur :: QuadTree -> QuadTree
blur Black = Black
blur White = White
blur (Combine q1 q2 q3 q4) = clockwise (flips q1 "1" (getListOfCoord (Combine q1 q2 q3 q4) "") (getListOfCoord                     (Combine q1 q2 q3 q4) "") (Combine q1 q2 q3 q4) )

                                    (flips q2 "2" (getListOfCoord (Combine q1 q2 q3 q4) "") (getListOfCoord(Combine q1 q2 q3 q4) "") (Combine q1 q2 q3 q4) )
                                    
                                    (flips q3 "3" (getListOfCoord (Combine q1 q2 q3 q4) "") (getListOfCoord (Combine q1 q2 q3 q4) "") (Combine q1 q2 q3 q4) )
                                    
                                    (flips q4 "4" (getListOfCoord (Combine q1 q2 q3 q4) "") (getListOfCoord     (Combine q1 q2 q3 q4) "") (Combine q1 q2 q3 q4) )
                        
