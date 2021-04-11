--Gilberto e Arthur

bin2dec :: [Int]->Int
bin2dec [] = 0
bin2dec [x] = x
bin2dec (x:xs) = bin2dec xs + x*2^length xs

dec2bin :: Int-> [Int]
dec2bin x = if x>2 then
            dec2bin(x`div`2)++[x `mod` 2]
            else 
                if x==2 then [1,0] 
                else
                   if x==1 then[1]
                   else [0] 

        
        