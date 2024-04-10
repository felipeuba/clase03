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
digitoUnidades x = mod x 10



--comparar :: Int -> Int -> Int