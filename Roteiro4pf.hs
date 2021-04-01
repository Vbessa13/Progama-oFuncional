--1
1:[2,3,4]
=>[1,2,3,4]
'a':['b','c','d']
=>"abcd"
head [1,2,3]
=>1
tail [1,2,3]
=>[2,3]
[1,5,2,3]!!1
=>5
[1,5,2,3]!!3
=>3
elem 2 [1,5,2,3]
=>true
take 2 [1,5,2,3,7]
=>[1,5]
drop 2 [1,5,2,3,7]
=>[2,3,7]
[1,2] ++ [3,4]
=>[1,2,3,4]
[1..10]
=>[1,2,3,4,5,6,7,8,9,10]
[7,6..3]
=>[7,6,5,4,3]
['b'..'g']
=>"bcdefg"
take 5 [1,3..]
=>[1,3,5,7,9]
sum [1..10]
=>55
maximum [1,5,2,3,7]
=>7
minimum [1,5,2,3,7]
=>1

--2

a) [5..1]

b) lista1 = ["a"+n | n <- [1,2,4]]
 
c)listaSimples = [(x)+3 | x <- [-2,1,4,7,10,13]]

d)

--3






--4

lst1=[x*2 | x <- [1..10], x*2 >= 12]
=> [12,14,16,18,20]  
lst2=[ x | x <- [50..100], mod x 7 == 3]
=> [52,59,66,73,80,87,94]   
lst3=[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
=> [10,11,12,14,16,17,18,20]   
lst4=[(x,y)| x <- [1..4], y <- [x..5]]
=> [(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5),(4,4),(4,5)]

--5

quadrados::Int->Int->[Int]
quadrados a b = [x*x | x <- [a..b]]

--6

selecionaImpares::[Int]->[Int]
selecionaImpares lista = [x | x <- lista, not (even x)]

--7

tabuada::Int->[Int]
tabuada x = [x*y | y <- [1..10]]

--8

bissexto::Int->Bool
bissexto x
  |mod x 4 == 0 && mod x 100 /= 0 = True
  |mod x 4 == 0 && mod x 100 == 0 && mod x 400 == 0 = True
  |otherwise = False

bissextos::[Int]->[Int]
bissextos lista = [x | x <- lista, bissexto x]

--9

sublistas::[[Int]]->[Int]
sublistas lista = [x | sub<-lista, x<-sub]

--10

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo =
[("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

bissexto::Integer->Bool
bissexto x
  |mod x 4 == 0 && mod x 100 /= 0 = True
  |mod x 4 == 0 && mod x 100 == 0 && mod x 400 == 0 = True
  |otherwise = False

valida::Data->Bool
valida (d,m,a)
  |d <= 0 || d > 31 = False
  |m <= 0 || m > 12 = False
  |a < 0 = False
  |((m == 4)||(m == 6)||(m == 9)||(m == 11)) && d > 30 = False
  |(bissexto a) && (m == 2) && (d > 29) = False
  |not(bissexto a) && (m == 2) && (d > 28) = False
  |otherwise = True

precede::Data->Data->Bool
precede(d1,m1,a1) (d2,m2,a2)
  |not(valida(d1,m1,a1)) = False
  |not(valida(d2,m2,a2)) = False
  |a1 < a2 = True
  |a1 == a2 && m1 < m2 = True
  |a1 == a2 && m1 == m2 && d1 < d2 = True
  |otherwise = False

emDia::Data->Emprestimo->Bool
emDia dataAtual (livro, aluno, dataRetirada, dataDevolucao, status) =
  precede dataAtual dataDevo

atrasados::Emprestimos->Data->[Emprestimos]
atrasados lista dataAtual = [x | x <- lista, not (emDia dataAtual x)]

--11

uniaoNRec::[Int]->[Int]
uniaoNRec lista = 

uniaoRec [] [] = []
uniaoRec (x:xs) (y:ys)
  |x == y = x:uniaoRec xs ys
  |otherwise = uniaoRec xs ys
