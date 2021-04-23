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
dec2bin 0 = [0]
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
bincompl2dec :: [Int] -> Int  
bincompl2dec [] = 0                 
bincompl2dec xs = if (head xs)==1 then 
                        negate(bin2dec(compl2 xs)) 
                    else bin2dec(compl2 xs)


    --[1,0,1,1,0] => negate(bin2dec(compl2 [1,0,1,1,0])))
    --negate(bin2dec(compl2 [1,0,1,1,0])))
    --negate(bin2dec(compl2 ([1,0,1,1])++[0])
    --negate(bin2dec(reverse(inverter(tail (reverse xs)))++[1]++[0]))
    --negate(bin2dec(reverse(inverter(tail ([1,1,0,1])))++[1]++[0]))
    --negate(bin2dec(reverse(inverter([1,0,1])))++[1]++[0]))
    --negate(bin2dec(reverse([0]++inverter [0,1])))++[1]++[0]))
    --negate(bin2dec(reverse([0]++[1]++inverter [1])))++[1]++[0]))
    --negate(bin2dec(reverse([0]++[1]++0]++inverter [])))++[1]++[0]))
    --negate(bin2dec(reverse([0,1,0])))++[1]++[0]))
    --negate(bin2dec([0,1,0])++[1]++[0])
    --negate(bin2dec([0,1,0,1,0]) => bin2dec xs + x*2^length xs
    --negate(bin2dec [1,0,1,0] + 0*2^length 4)
    --negate(bin2dec [1,0,1,0] + 0)
    --negate(bin2dec [0,1,0] + 1*2^length 3 + 0)
    --negate(bin2dec [0,1,0] + 8 + 0)
    --negate(bin2dec [1,0] + 0*2^length 2 + 8 + 0)
    --negate(bin2dec [1,0] + 0 + 8 + 0)
    --negate(bin2dec [0] + 1*2^length 1 + 0 + 8 + 0)
    --negate(bin2dec [0] + 2 + 0 + 8 + 0)
    --negate(bin2dec [] + 0*2^length 0 + 2 + 0 + 8 + 0)
    --negate(0 + 0 + 2 + 0 + 8 + 0)
    --negate(10)
    -- -10


----------------------

--4
--OBS: Somente decimais positivos serão passados para seu valor equivalente em complemento de dois
dec2bincompl :: Int->[Int]
dec2bincompl 0 = [0]
dec2bincompl x = (compl2(dec2bin x))

-- 4 => (compl2(dec2bin x))
-- (compl2(dec2bin 4))
-- (compl2(dec2bin(4`div`2)++[4 `mod` 2]))
-- (compl2([1,0]++[0]))
-- compl2([1,0,0])
-- compl2([1,0])++[0]
-- compl2([1])++[0]++[0]
-- reverse(inverter(tail (reverse [])))++[1]++[0]++[0]
-- reverse(inverter(tail ([])))++[1]++[0]++[0]
-- []++[1]++[0]++[0]
-- [1,0,0] (SEM BIT DE SINAL)

---------------------------------------------------
--PARA COMPLEMENTO DE 2 sem o bit de sinal
compl2 :: [Int]->[Int]
compl2 xs = if (last xs)==1 then reverse(inverter(tail (reverse xs)))++[1] else compl2(init xs)++[0]


--inverte bit a bit
inverter :: [Int]->[Int]
inverter [] = []
inverter (x:xs) = if x==1 then 
                    [0]++inverter xs 
                   else [1]++inverter xs

                   
--------------------------------------------------

--5
somarbin :: [Int]->[Int]->[Int]
somarbin [] [] = []
somarbin xs [] = xs
somarbin [] xs = xs
somarbin (xs) (ys) =  dec2bin ((bin2dec(compl2 xs)) + (bin2dec(compl2 ys)))


-- A função funciona normalmente, porém sua demonstração por recursão se torna extensa por executar a chamada de outras recursoes que fazem recusoes ...


--somarbin [0,1,1,0][1,0,1] =>  dec2bin ((bin2dec(compl2 xs)) + (bin2dec(compl2 ys)))
--dec2bin ((bin2dec(compl2 [0,1,1,0])) + (bin2dec(compl2 [1,0,1])))
--dec2bin ((bin2dec(compl2(init [0,1,1,0])++[0])) + (bin2dec(reverse(inverter(tail (reverse [1,0,1])))++[1])))
--dec2bin ((bin2dec(compl2([0,1,1])++[0])) + (bin2dec(reverse(inverter(tail ([1,0,1])))++[1])))
--dec2bin ((bin2dec(reverse(inverter(tail (reverse [0,1,1])))++[1]++[0])) + ...
--dec2bin ((bin2dec(reverse(inverter(tail [1,1,0]))++[1]++[0])) + ...
--dec2bin ((bin2dec(reverse(inverter[1,0])++[1]++[0])) + ....
--dec2bin ((bin2dec(reverse[0,1]++[1]++[0])) + ....
--dec2bin ((bin2dec([1,0]++[1]++[0])) + ....
--dec2bin ((bin2dec[1,0,1,0]) + ......
-- ...
-- [1,1,0,1]



-- ------------------


subtrairbin :: [Int]->[Int]->[Int]
subtrairbin [] [] = []
subtrairbin xs [] = xs
subtrairbin [] xs = []
subtrairbin (xs) (ys) =  dec2bin ((bin2dec(compl2 xs)) - (bin2dec(compl2 ys)))




--7
andbin :: [Int] -> [Int] -> [Int]
andbin [] [] = []
andbin  xs [] = xs
andbin [] xs = xs
andbin (x:xs) (y:ys) = if x==1 && y==1 then [1]++andbin xs ys else [0]++andbin xs ys

--andbin [1,0,1][0,1,1] => [0]++andbin xs ys
--[0]++andbin [0,1] [1,1]
--[0]++[0]++andbin [1] [1]
--[0]++[0]++[1]++andbin xs ys
--[0]++[0]++[1]++andbin [] []
--[0,0,1]

--8    
orbin :: [Int] -> [Int] -> [Int]
orbin [] [] = []
orbin  xs [] = xs
orbin [] xs = xs
orbin (x:xs) (y:ys) = if x==1 || y==1 then [1]++orbin xs ys else [0]++orbin xs ys

--orbin [1,0,1] [0,1,1] => [1]++orbin xs ys
--[1]++orbin [0,1] [1,1]
--[1]++[1]++orbin [1] [1]
--[1]++[1]++[1]++orbin [] []
--[1,1,1]


--------------
--9
frac2bin :: Double -> ([Int],[Int])
frac2bin x = if (x - (fromIntegral(floor x))) > 0 then 
                    (dec2bin(floor x),dec2bin(multdez(x - (fromIntegral(floor x)))))
            else (dec2bin(floor x),[0])

--frac2bin 10.25 => if (10.25 - (fromIntegral(floor 10.25))) > 0
--    frac2bin 10.25 => if (10.25 - fromIntegral 10) > 0
--        frac2bin 10.25 => if 0.25 > 0
--(dec2bin(floor 10.25),dec2bin(multdez(10.25 - (fromIntegral(floor 10.25)))))
--(dec2bin(10),dec2bin(multdez(10.25 - (fromIntegral 10))))
--([1,0,1,0],dec2bin(multdez(10.25 - 10)))
--([1,0,1,0],dec2bin(multdez(0.25))) => multdez (0.25 - (fromIntegral(floor 0.25))) > 0
--([1,0,1,0],dec2bin(multdez(0.25))) => multdez (0.25 - 0) > 0
--([1,0,1,0],dec2bin(multdez (0.25*10)))
--([1,0,1,0],dec2bin(multdez (2.5*10)))
--([1,0,1,0],dec2bin 25)  => multdez -> else floor y
--([1,0,1,0],[])

multdez :: Double -> Int
multdez y = if (y - (fromIntegral(floor y))) > 0 then multdez (y*10) else floor y


-------------
--10
bin2frac ::([Int],[Int]) -> Double
bin2frac (x,y) = (fromIntegral(bin2dec x)) + (divdez(fromIntegral(bin2dec y)))

--bin2frac ([1,0,1,0][1,0,1]) => (fromIntegral(bin2dec x)) + (divdez(fromIntegral(bin2dec y)))
--(fromIntegral(bin2dec [1,0,1,0])) + (divdez(fromIntegral(bin2dec [1,0,1])))
--(fromIntegral 10) + (divdez(fromIntegral 5))
--10 + (divdez 5)
--10 + divdez(5/10) -- if>=1
--10 + divdez 0.5 -- else (y/1) para nao chamar recursao
--10 + 0.5
--10.5

divdez :: Double -> Double
divdez y = if (y>=1) then divdez(y/10) else (y/1)