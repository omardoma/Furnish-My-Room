import FurnitureResources
import Data.List (sortBy)
 
findFurnitureUpdate :: [Char] -> [Char] -> [Char] -> [([Char],[[([Char],[Char],Int)]])] -> [([Char],[[([Char],[Char],Int)]])]

findFurnitureUpdate a b c []= if c== "right" then [(a,[[(b,c,1)],[]])] else [(a,[[],[(b,c,1)]])]
findFurnitureUpdate a b c l@(pair@(x,list1@[rl,bl]):xs)= if x==a then if c=="right" 
then (x,replace rl ((findandUpdateTerm b c ((findlist a l) !! 0))) list1):xs 
else (x,replace bl ((findandUpdateTerm b c ((findlist a l ) !! 1))) list1):xs
else pair:findFurnitureUpdate a b c xs

replace a b (x:xs)= if a==x then b:xs else x:replace a b xs 

findlist a []= error "not found "
findlist a ((x,y):xs)= if x==a then y else findlist a xs 

findandUpdateTerm :: (Eq a, Eq b, Num c) => b -> a -> [(b,a,c)] -> [(b,a,c)]
findandUpdateTerm b c []= [(b,c,1)]
findandUpdateTerm b c ((b1,c1,f):xs)=if b==b1 && c==c1 then ((b1,c1,(f+1)):xs) else (b1,c1,f):findandUpdateTerm b c xs

generate :: [[[Char]]] -> [([Char],[[([Char],[Char],Int)]])] ->[([Char],[[([Char],[Char],Int)]])]
generateBelow :: [[[Char]]] -> [([Char],[[([Char],[Char],Int)]])] ->[([Char],[[([Char],[Char],Int)]])]
generateRight:: [[[Char]]] -> [([Char],[[([Char],[Char],Int)]])] ->[([Char],[[([Char],[Char],Int)]])]

generateRight [] statsList= statsList
generateRight ([]:xs) statsList = generateRight xs statsList
generateRight ([a]:xs) statsList= generateRight xs statsList
generateRight ((a:b:ax):xs) statsList = generateRight ((b:ax):xs) (findFurnitureUpdate a b "right" statsList)

generateBelow [] statsList = statsList 
generateBelow ([]:xs) statsList = generateBelow xs statsList
generateBelow [x] statsList = statsList 
generateBelow (l1:l2:z) statsList= generateBelow (l2:z) (updateStats l1 l2 statsList)

updateStats [] [] statsList= statsList 
updateStats (x:xs) (y:ys) statsList = updateStats xs ys (findFurnitureUpdate x y "below" statsList)

generate room statsList = generateBelow room (generateRight room statsList)
generate [] statsList=statsList 


statsList = createStatsList training []

createStatsList(room:rooms) s = createStatsList (rooms) (generate room s)
createStatsList [] s = organizeForSorting s

organizeForSorting []=[]
organizeForSorting ((element,[rightList,belowList]):xs) = ((element,[(sortBy cmp rightList),(sortBy cmp belowList)]):organizeForSorting xs)

cmp (a1, b1, c1) (a2, b2, c2) = compare c2 c1

--Generation phase 
getFurnStat :: [Char] -> [[([Char],[Char],Int)]]
getFurnStat furn = findlist furn statsList


getPossibleNeighbour :: [([Char],[Char],Int)] -> [([Char],[Char],Int)] -> [Char]
getPossibleNeighbour l1 l2 = if (randomZeroToX 1)==0 then  extract (l1 !!(randomZeroToX ((length l1)-1)))
else extract (l2 !! (randomZeroToX ((length l2)-1)))

extract (a,_,_)=a 

furnishRoom :: Int -> [Char] -> [[[Char]]]
furnishRoom 0 str = []
furnishRoom i str = furnishRoom3 i str i 

furnishRoom3 0 str j = []
furnishRoom3 i str j= (constructSubList j str):(furnishRoom3 (i-1) str j) 

constructSubList :: Num a => a -> [Char] -> [[Char]]
constructSubList 0 str = []
constructSubList j str =  (helper str: constructSubList (j-1) str) 

helper :: [Char] -> [Char]
helper str = getPossibleNeighbour ((getFurnStat str) !! 0) ((getFurnStat str) !!1) 

