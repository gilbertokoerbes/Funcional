--Gilberto e Arthur
--1
bin2dec :: [Int]->Int
bin2dec [] = 0
bin2dec [x] = x
bin2dec (x:xs) = bin2dec xs + x*2^length xs

--2
dec2bin :: Int-> [Int]
dec2bin x = if x>2 then
                dec2bin(x`div`2)++[x `mod` 2]
            else 
                if x==2 then [1,0] 
                else
                   if x==1 then[1]
                   else [0] 

--4
dec2bincompl :: Int->[Int]
dec2bincompl x = (auxCompl1(dec2bin x))

--inverte bit a bit
auxCompl1 :: [Int]->[Int]
auxCompl1 [] = []
auxCompl1 (x:xs) = if x==1 then 
                    [0]++auxCompl1 xs 
                   else [1]++auxCompl1 xs

auxComp2 :: Int->[Int]->[Int]
auxComp2 x [] = []
auxComp2 vaiUm [xs] = auxComp2 vaiUm init xs(if (tail xs+1)>1 then vaiUm=0+1 else vaiUm=0+0)

--auxComp2 -> vaiUm - 
--auxComp2 -> if head(reverse xs) = 0 then result = 1, vaiUm = 0
--            if head(reverse xs) = 1 then result = 0, vaiUm = 1
--  


--andbin :: [Int] -> [Int] -> [Int]
--andbin [] ys = ys
--andbin [] xs = xs

--andbin (x:xs) (y:ys) = if x==1 && y==1 then [1]++andbin [x]
--                       else [0]++andbin [x]

    
orbin :: [Int] -> [Int] -> [Int]
orbin [] ys = ys
orbin [] xs = xs

orbin (x:xs) = if x==0 && y==0 then x : (orbin [0])
                      else x : (orbin [1])

        