import FurnitureResources
replace a b (x:xs)= if a==x then b:xs else x:replace a b xs 
 
findFurnitureUpdate :: [Char] -> [Char] -> [Char] -> [([Char],[[([Char],[Char],Int)]])] -> [([Char],[[([Char],[Char],Int)]])]

 
findFurnitureUpdate a b c []= error"mafish"
findFurnitureUpdate a b c l@(e@(x,list1@[rl,bl]):xs)= if x==a then if c=="right" 
then (x,replace rl ((findandUpdateTerm b c ((findlist a l) !! 0))) list1):xs 
else (x,replace bl ((findandUpdateTerm b c ((findlist a l ) !! 1))) list1):xs
else e:findFurnitureUpdate a b c xs


findlist a []= error "not found "
findlist a ((x,y):xs)= if x==a then y else findlist a xs 

findandUpdateTerm b c []= [(b,c,1)]++[]
findandUpdateTerm b c ((b1,c1,f):xs)=if b==b1 && c==c1 then ((b1,c1,(f+1)):xs) else (b1,c1,f):findandUpdateTerm b c xs

--getFurnStat :: [Char] -> [[([Char],[Char],Int)]]
--getFurnStat s = 

--furnishRoom :: Int -> [Char] -> [[[Char]]]
--furnishRoom i f = 


--help ((x:xs):(y:ys))= 

generate :: [[[Char]]] -> [([Char],[[([Char],[Char],Int)]])] ->[([Char],[[([Char],[Char],Int)]])]


generate [] statslist = statslist
generate ([a]) statslist =statslist
generate ([]:xs) statslist =generate xs statslist 

generate (l1@(x:xs):l2@(y:ys):zs) statslist = if length l1 >1 then generate (x:y:xs) (findFurnitureUpdate (x!!0) (y!!0) "below" (findFurnitureUpdate (x!!0) (x!!1) "right" statslist))
else generate (y:xs) (findFurnitureUpdate (x!!0) (y!!0) "below" statslist)

generate ((a:b:as):xs) statslist =if xs/=[] then generate ((b:as):xs) (findFurnitureUpdate a ((xs!!0)!!0) "below"(findFurnitureUpdate a b "right" statslist))
else generate ((b:as):xs) (findFurnitureUpdate a b "right" statslist)


--generate ([a]:[b]:xs) statslist = generate xs (findFurnitureUpdate a b "below" statslist)
generate ([a]:xs) statslist = generate xs (findFurnitureUpdate a ((xs!!0)!!0) "below" statslist )


