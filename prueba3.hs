-- problema todoMenor (x,y : <R x R>) : Boolean{
--requiere {true}
--asegura {(x0<y0) y (x1 < y1) si y solo si true}
--}

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor x y = fst x < fst y && snd x < snd y


distanciaDosPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaDosPuntos x y = sqrt((fst y - fst x) *  (fst y - fst x) + (snd y - snd x) * (snd y - snd x))

--4)f)
--problema posPrimerPar (x,y : Z x Z x Z) : N{
--requiere{True}
--{si existe par, res = a su posicion, sino devuelve 4}
--}

posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (a, b, c)  | mod a 2 == 0 = 0
                       | mod b 2 == 0 = 1
                       | mod c 2 == 0 = 2
                       | otherwise = 4


--problema bisiesto (a˜no: Z) : Bool {
--requiere: {True}
--asegura: {res=false ↔ ano no es multiplo de 4 o ano es
--multiplo de 100 pero no de 400}
--}

bisiesto :: Int -> Bool
bisiesto x | mod x 100 == 0 = mod x 400 == 0 
           | otherwise = mod x 4 == 0
           
absoluto :: Float -> Float
absoluto x | x >= 0 = x
           | otherwise = -x


distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (a,b,c) (d,e,f) =  absoluto (a-d) + absoluto (b - e) + absoluto (c-f)

digitoUnidades :: Int -> Int
digitoUnidades x | x >=0 = mod x 10
                 |otherwise = mod (-x) 10

digitoDecena :: Int -> Int
digitoDecena x | x >=0 = div x 10
               |otherwise = div (-x) 10


sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos x = digitoDecena(x) + digitoUnidades(y)

comparar :: Int -> Int -> Int
comparar x y | sumaUltimosDosDigitos(x) < sumaUltimosDosDigitos(y) = 1
             | sumaUltimosDosDigitos(x) > sumaUltimosDosDigitos(y) = -1
             | otherwise = 0


--1

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--2

parteEntera :: Float -> Integer
parteEntera x | 0 <= x && x < 1 = 0
              | otherwise = 1 + parteEntera (x-1)

--3 

esDivisible :: Int -> Int -> Bool
esDivisible x y | x == y  = True
                | x < y = False
                | otherwise = esDivisible (x-y) y

--4

sumaImpares :: Integer ->Integer
sumaImpares x | x == 1 = 1
              | otherwise = (2 * x - 1) + sumaImpares (x-1)

--5 

medioFact :: Integer -> Integer
medioFact x | x == 1 || x == 0 = 1
            | x == 2 = 2
            | otherwise = x * medioFact (x-2)

--6

sumaDigitos :: Int -> Int
sumaDigitos x | x < 10 = x
              | otherwise = mod x 10 + sumaDigitos (div x 10)

--7

todosDigitosIguales :: Integer ->Bool
todosDigitosIguales x | x < 10 = True
                      | otherwise = mod x 10 == mod (div x 10) 10 && todosDigitosIguales (div x 10)

--8

cantidadDeDigitos :: Integer -> Integer 
cantidadDeDigitos x | x < 10 = x
                    | otherwise = 1 + cantidadDeDigitos(div x 10)

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito x i | cantidadDeDigitos x == i = mod x 10
                 | otherwise = iesimoDigito(div x 10) i

--9

