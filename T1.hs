--Gilberto e Arthur
--1
bin2dec :: [Int]->Int
bin2dec [] = 0
bin2dec [x] = x
bin2dec (x:xs) = bin2dec xs + x*2^length xs

--bin2dec [1,0,0] => bin2dec xs + x*2^length xs
--bin2dec [0,0] + 1*2^length 2
--bin2dec [0,0] + 4
--bin2dec 0 + 0*2^length 1 + 4
--bin2dec [] + 0*2^length 0 + 0 + 4
--bin2dec 0 + 0 + 0 + 4
--4

---------------------------
--2
dec2bin :: Int-> [Int]
dec2bin x = if x>2 then
                dec2bin(x`div`2)++[x `mod` 2]
            else 
                if x==2 then [1,0] 
                else
                   if x==1 then[1]
                   else [0] 

-- 10 => dec2bin(x`div`2)++[x `mod` 2]
-- dec2bin(10`div`2)++[10 `mod` 2]
-- dec2bin(5)++[0]
--dec2bin(5`div`2)++[5 `mod` 2]++[0]
--dec2bin(2)++[1]++[0]
--[1,0]++[1]++[0]
--[1,0,1,0]

-----------------------------
--3 
bincompl2dec ::[Int] -> Int                   
bincompl2dec xs = if (head xs)==1 then 
                        negate(bin2dec(compl2 xs)) 
                    else bin2dec(compl2 xs)

--[0,1,1,0] => bin2dec(compl2 [0,1,1,0])
--bin2dec(compl2 [0,1,1,0])
--bin2dec(compl2([0,1,1])++[0])
--bin2dec(reverse(inveter(tail (reverse xs)))++[1]++[0])
--bin2dec(reverse(inveter(tail ([1,1,0])))++[1]++[0])
--bin2dec(reverse(inveter([1,0]))++[1]++[0])
--bin2dec(reverse([0]++inveter [0])++[1]++[0])
--bin2dec(reverse([0]++[1]++inveter [])++[1]++[0])
--bin2dec(reverse([0]++[1]++[])++[1]++[0])
--bin2dec(reverse[0,1]++[1]++[0])
--bin2dec([1,0]++[1]++[0])
--bin2dec([1,0,1,0]) => bin2dec (x:xs) = bin2dec xs + x*2^length xs
--bin2dec [0,1,0] + 1*2^length 3
--bin2dec [1,0] + 8
--bin2dec [0] + 1*2^length 1 + 8
--bin2dec [] + 0*2^length 0 + 2 + 8
--bin2dec [] + 0 + 2 + 8
--0 + 0 + 2 + 8
-- 10


----------------------

--4
--OBS: Somente decimais positivos serão passados para seu valor equivalente e complemento de dois
dec2bincompl :: Int->[Int]
dec2bincompl x = (compl2(dec2bin x))

---------------------------------------------------
--PARA COMPLEMENTO DE 2 sem o bit de sinal
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
somarbin (xs) (ys) =  dec2bin ((bin2dec(compl2 xs)) + (bin2dec(compl2 ys)))

subtrairbin :: [Int]->[Int]->[Int]
subtrairbin (xs) (ys) =  dec2bin ((bin2dec(compl2 xs)) - (bin2dec(compl2 ys)))



--xorbin
--xorbin :: Int -> Int ->Int
--xorbin q w = if q==w then 0 else 1
--


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



--------------
--9
frac2bin :: Double -> ([Int],[Int])
--frac2bin x = if (x - (fromIntegral(floor x))) > 0 then (dec2bin(floor x),[0]) else ([0],[0])
frac2bin x = if (x - (fromIntegral(floor x))) > 0 then 
                    (dec2bin(floor x),dec2bin(multdez(x - (fromIntegral(floor x)))))
            else (dec2bin(floor x),[0])


multdez :: Double -> Int
--posvirgula y = floor y
multdez y = if (y - (fromIntegral(floor y))) > 0 then multdez (y*10) else floor y
-------------

--10
bin2frac ::([Int],[Int]) -> Double
bin2frac (x,y) = (fromIntegral(bin2dec x)) + (divdez(fromIntegral(bin2dec y)))


divdez :: Double -> Double
divdez y = if (y>=1) then divdez(y/10) else (y/1)