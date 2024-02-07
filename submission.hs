data QuadTree  = Black | White | Combine QuadTree QuadTree QuadTree QuadTree deriving(Eq, Show)

allBlack :: Int -> QuadTree
allBlack z = Black

allWhite :: Int -> QuadTree
allWhite z = White

clockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
clockwise Black Black Black Black = Black
clockwise White White White White = White
clockwise  q1 q2 q3 q4  = Combine q1 q2 q3 q4 

anticlockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
anticlockwise Black Black Black Black = Black
anticlockwise White White White White = White
anticlockwise  q1 q2 q3 q4  =  Combine q1 q4 q3 q2 

getListOfCoord :: QuadTree -> String -> [String]
getListOfCoord Black s = [s]
getListOfCoord White s = [s]
getListOfCoord (Combine q1 q2 q3 q4) s =
  (getListOfCoord q1 (s ++ "1")) ++
  (getListOfCoord q2 (s ++ "2")) ++
  (getListOfCoord q3 (s ++ "3")) ++
  (getListOfCoord q4 (s ++ "4"))
  
  
getColor :: QuadTree -> String -> QuadTree
getColor q [] = q
getColor (Combine q1 q2 q3 q4) ('1':s) = getColor q1 s
getColor (Combine q1 q2 q3 q4) ('2':s) = getColor q2 s
getColor (Combine q1 q2 q3 q4) ('3':s) = getColor q3 s
getColor (Combine q1 q2 q3 q4) ('4':s) = getColor q4 s
  
checkSameColor :: QuadTree -> QuadTree -> Bool
checkSameColor q1 q2 = q1 == q2


checkNeighbour :: String -> String -> Bool
checkNeighbour  [] [] = False
checkNeighbour s "" = False
checkNeighbour "" s = False

checkNeighbour (x:s1) (y:s2) =
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

checkSecNeb :: String -> String -> String -> Bool
checkSecNeb [] [] s = True


checkSecNeb (x:s1) [] "h" = (read [x]  == 2 || read [x] == 3) && checkSecNeb s1 [] "h"
checkSecNeb [] (x:s1) "h" = (read [x]  == 1 || read [x] == 4) && checkSecNeb [] s1 "h"


checkSecNeb (x:s1) [] "v" = (read [x]  == 3 || read [x] == 4) && checkSecNeb s1 [] "v"
checkSecNeb [] (x:s1) "v" = (read [x]  == 1 || read [x] == 2) && checkSecNeb [] s1 "v"

checkSecNeb (x:s1) (y:s2) "h" =  (read [x] == 2 || read [x] == 3) && ((read [x] + read [y]) == 3 || (read [x] + read [y]) == 7) && checkSecNeb s1 s2 "h"
checkSecNeb (x:s1) (y:s2) "v" = (read [x] == 3 || read [x] == 4) && (read [x] + read [y]) == 5 && checkSecNeb s1 s2 "v"

checkSecNeb _ _ _ = False






add :: QuadTree -> String -> [String] -> [String] -> QuadTree -> Int
add q1 s [] cl pq = 0
add q1 s (s1:sl) cl pq = if (checkNeighbour s s1 && (checkSameColor q1 (getColor pq s1)) ) 
                            then  (1 + (add q1 s sl cl pq))
                        else if (checkNeighbour s s1 && (not (checkSameColor q1 (getColor pq s1))) ) 
                            then (-1 + (add q1 s sl cl pq))
                         else add q1 s sl cl pq



flips :: QuadTree -> String -> [String] -> [String] -> QuadTree -> QuadTree
flips Black s sl cl pq = if (add Black s sl cl pq) < 0 then White
                        else Black
flips White s sl cl pq = if (add White s sl cl pq) < 0 then Black
                        else White
flips (Combine q1 q2 q3 q4) s sl cl pq = clockwise (flips q1 (s++"1") sl cl pq) (flips q2 (s++"2") sl cl pq) (flips q3 (s++"3") sl cl pq) (flips q4 (s++"4") sl cl pq)






blur :: QuadTree -> QuadTree
blur Black = Black
blur White = White
blur (Combine q1 q2 q3 q4) = clockwise (flips q1 "1" (getListOfCoord (Combine q1 q2 q3 q4) "") (getListOfCoord                     (Combine q1 q2 q3 q4) "") (Combine q1 q2 q3 q4) )

                                    (flips q2 "2" (getListOfCoord (Combine q1 q2 q3 q4) "") (getListOfCoord(Combine q1 q2 q3 q4) "") (Combine q1 q2 q3 q4) )
                                    
                                    (flips q3 "3" (getListOfCoord (Combine q1 q2 q3 q4) "") (getListOfCoord (Combine q1 q2 q3 q4) "") (Combine q1 q2 q3 q4) )
                                    
                                    (flips q4 "4" (getListOfCoord (Combine q1 q2 q3 q4) "") (getListOfCoord     (Combine q1 q2 q3 q4) "") (Combine q1 q2 q3 q4) )
                        
            

                                    
                                    
                                    
                                    
-- main = print(add White
--   "113" (getListOfCoord
--   (Combine (Combine (Combine Black Black White Black) (Combine Black Black Black White) White Black) (Combine Black                                     Black 
--                                     (Combine Black Black Black White)
--                                     (Combine Black Black White White)) 
--                           White 
--                           (Combine 
--                                     Black 
--                                     (Combine Black Black White Black)
--                                     (Combine Black White White Black) 
--                                     Black)) "") (getListOfCoord
--   (Combine (Combine (Combine Black Black White Black) (Combine Black Black Black White) White Black) (Combine Black                                     Black 
--                                     (Combine Black Black Black White)
--                                     (Combine Black Black White White)) 
--                           White 
--                           (Combine 
--                                     Black 
--                                     (Combine Black Black White Black)
--                                     (Combine Black White White Black) 
--                                     Black)) "") 
                                    
--                                     (Combine (Combine (Combine Black Black White Black) (Combine Black Black Black White) White Black) (Combine Black                                     Black 
--                                     (Combine Black Black Black White)
--                                     (Combine Black Black White White)) 
--                           White 
--                           (Combine 
--                                     Black 
--                                     (Combine Black Black White Black)
--                                     (Combine Black White White Black) 
--                                     Black)))
                                    
-- main = print(checkNeighbour "113" "121" )
--["111","112","113","114","121","122","123","124","13","14",
--"21","22","231","232","233","234","241","242","243","244",
--"3","41","421","422","423","424","431","432","433","434","44"]


-- main = print(getListOfCoord
-- (Combine (Combine (Combine Black Black White Black) (Combine Black Black Black White) White Black) 
-- (Combine Black 
--                                     Black 
--                                     (Combine Black Black Black White)
--                                     (Combine Black Black White White)) 
--                           White 
--                           (Combine 
--                                     Black 
--                                     (Combine Black Black White Black)
--                                     (Combine Black White White Black) 
--                                     Black)) "" )
                                    
                                    