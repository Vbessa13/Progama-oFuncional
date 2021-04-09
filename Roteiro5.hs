--1

conta_ch:: [Char] -> Int
conta_ch [] = 0
conta_ch (x:resto) = 1 + conta_ch resto

conta:: [t] -> Int
conta[] = 0
conta (x:r) = 1 + conta r

maior:: [Int} -> Int
maior [x] = x
maior (x:y:resto)
    | x > y = maior (x:resto)
    | otherwise = maior (y:resto)

primeiros::Int -> [t] -> [t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x: primeiros (n-1) xs

pertence:: t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = if (a == x) then True
                               else pertence a z

uniaoR:: [t] -> [t] -> [t]
uniaoR [] L = L
uniaoR (x:xs) L = if pertence x L then uniaoR xs L
                                  else x: uniaoR xs L


--2

npares::[Int]->Int
npares [] = 0
npares (x:xs)
  |even x = 1 + (npares xs)
  |otherwise = (npares xs)


--3

produtorio::[Int]->Int
produtorio [x] = x
produtorio (x:xs) = x * (produtorio xs)

--4

comprime::[[Int]]->[Int]
comprime [] = []
comprime (x:xs) = x ++ comprime xs

--5

tamanho::Num a=>[a]->a --polimorfica
tamanho [] = 0
tamanho (x:xs) = 1 + (tamanho xs)


--6

uniaoRec [] [] = []
uniaoRec (x:xs) (y:ys)
  |x == y = x:uniaoRec xs ys
  |otherwise = uniaoRec xs ys
