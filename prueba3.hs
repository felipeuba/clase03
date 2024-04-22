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
--9
 --UltimoDigito div x 10^(cantidadDeDigitos-1) asi voy obteniendo los primeros digitos
            -- despues los tengo que comparar con los ultimos

esCapicua :: Integer -> Bool
esCapicua x | cantidadDeDigitos x == 1 = True
            | otherwise = mod (div x (10^(cantidadDeDigitos x - 1))) 10 == mod x 10
            
--10 son sumatorias todas
--a
f1 :: Int -> Int
f1 x | x==0 = 1
     | otherwise = 2^x + f1(x-1)

--b
f2 :: Int -> Float -> Float
f2 n q | n == 0 = 0
       | otherwise = q ^ n + f2 (n-1) q

--c
f3 :: Int -> Float -> Float
f3 n = f2 (2*n)

--d

f4 :: Int -> Float -> Float
f4 n q = f3 n q - f2 (n-1) q

--11


eAprox :: Int -> Float
eAprox n | n == 1 = 2
         | otherwise = 1 / fromIntegral (factorial n) + eAprox(n-1) 

--11 b

e :: Float
e = eAprox 9

--12

fg :: Integer -> Float
fg 1 = 2
fg n = 2 + 1 / fg (n-1)

raizDe2Aprox :: Integer ->Float
raizDe2Aprox x = fg (x-1)

--13

primerSum :: Int -> Int -> Int
primerSum n m | m == 1 = n
              | otherwise = n ^ m + primerSum n (m-1)
              
segundaSum :: Int -> Int -> Int
segundaSum n m | n == 1 = primerSum 1 m
               | otherwise = primerSum n m + segundaSum  (n-1) m

--14

sumaABPotencias :: Int -> Int -> Int -> Int
sumaABPotencias q a b | a == 1 = q^(a+b)
                      | otherwise = q ^ (a+b) + sumaABPotencias q (a-1) b


sumaPotencias :: Int -> Int ->Int ->Int
sumaPotencias q a b | b == 1 = sumaABPotencias q a b
                    | otherwise = sumaABPotencias q a b + sumaPotencias q a (b-1)

--15

primerSumaRacionales :: Int -> Int -> Float
primerSumaRacionales p q | q == 1 = fromIntegral p
                         | otherwise = fromIntegral p / fromIntegral q + primerSumaRacionales p (q-1)

sumaRacionales :: Int -> Int -> Float
sumaRacionales p q | p == 1 =  primerSumaRacionales p q
                   | otherwise = primerSumaRacionales p q + sumaRacionales (p-1) q


--16
contarDivisoresDeHasta :: Int -> Int -> Int
contarDivisoresDeHasta y x | y == x = 1
                           | mod x y == 0 = 1 + contarDivisoresDeHasta (y+1) x
                           | otherwise = contarDivisoresDeHasta (y+1) x

esPrimo :: Int ->Bool
esPrimo x = contarDivisoresDeHasta 1 x <= 2
