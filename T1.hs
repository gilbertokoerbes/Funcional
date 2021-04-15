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

--3
bincompl2dec ::[Int] -> Int                   
bincompl2dec xs = if (head xs)==1 then negate(bin2dec(compl2 xs)) else bin2dec(compl2 xs)

--4
--OBS: Somente decimais positivos serão passados para seu valor equivalente e complemento de dois
dec2bincompl :: Int->[Int]
dec2bincompl x = (compl2(dec2bin x))

---------------------------------------------------
--PARA COMPLEMENTO DE 2
compl2 ::[Int]->[Int]
compl2 xs = if (last xs)==1 then reverse(inveter(tail (reverse xs)))++[1] else compl2(init xs)++[0]


--inverte bit a bit
inveter :: [Int]->[Int]
inveter [] = []
inveter (x:xs) = if x==1 then 
                    [0]++inveter xs 
                   else [1]++inveter xs
--------------------------------------------------

--5
somarbin :: [Int]->[Int]->[Int]
somarbin (xs) (ys) =  andbin (compl2 xs)(compl2 ys)

--6



--7
andbin :: [Int] -> [Int] -> [Int]
andbin  xs [] = xs
andbin [] xs = xs
andbin (x:xs) (y:ys) = if x==1 && y==1 then [1]++andbin xs ys else [0]++andbin xs ys

--8    
orbin :: [Int] -> [Int] -> [Int]
orbin  xs [] = xs
orbin [] xs = xs
orbin (x:xs) (y:ys) = if x==1 || y==1 then [1]++orbin xs ys else [0]++orbin xs ys

--[10],[5] = ([1,0,1,0],[0])


--frac2bin :: Double -> ([Int],[Int])

