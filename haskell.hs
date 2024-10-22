-- Ejercicio 1: promedio3
promedio3 x y z = (x + y + z) / 3

-- Ejercicio 2: sumaMonedas
sumaMonedas a b c d e = a * 1 + b * 2 + c * 5 + d * 10 + e * 20

-- Ejercicio 3: volumenEsfera
volumenEsfera r = (4 / 3) * pi * r^3

-- Ejercicio 4: areaDeCoronaCircular
areaDeCoronaCircular r1 r2 = pi * (r2^2 - r1^2)

-- Ejercicio 5: ultimaCifra
ultimaCifra x = x `mod` 10

-- Ejercicio 6: maxTres
maxTres x y z = max x (max y z)

-- Ejercicio 7: rota1
rota1 xs = tail xs ++ [head xs]

-- Ejercicio 8: rota
rota n xs = drop n xs ++ take n xs

-- Ejercicio 9: rango
rango xs = [minimum xs, maximum xs]

-- Ejercicio 10: palindromo
palindromo xs = xs == reverse xs

-- Ejercicio 11: interior
interior xs = tail (init xs)

-- Ejercicio 13: segmento
segmento m n xs = take (n - m + 1) (drop m xs)

-- Ejercicio 14: extremos
extremos n xs = take n xs ++ drop (length xs - n) xs

-- Ejercicio 15: mediano
mediano x y z = x + y + z - maximum [x, y, z] - minimum [x, y, z]

-- Ejercicio 16: tresIguales
tresIguales x y z = x == y && y == z

-- Ejercicio 17: tresDiferentes
tresDiferentes x y z = x /= y && y /= z && x /= z

-- Ejercicio 18: cuatroIguales
cuatroIguales x y z u = tresIguales x y z && z == u

-- Ejercicio 1: divisionSegura
divisionSegura x y
  | y == 0    = 9999
  | otherwise = x / y

-- Ejercicio 2: xor1
xor1 True False = True
xor1 False True = True
xor1 _ _ = False

-- Ejercicio 3: mayorRectangulo
mayorRectangulo r1@(b1, h1) r2@(b2, h2)
  | b1 * h1 >= b2 * h2 = r1
  | otherwise = r2

-- Ejercicio 4: intercambia
intercambia (x, y) = (y, x)

-- Ejercicio 5: distancia
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- Ejercicio 6: ciclo
ciclo [] = []
ciclo xs = last xs : init xs

-- Ejercicio 7: numeroMayor
numeroMayor x y
  | x >= y    = x * 10 + y
  | otherwise = y * 10 + x

-- Ejercicio 8: numeroDeRaices
numeroDeRaices a b c
  | discriminante > 0 = 2
  | discriminante == 0 = 1
  | otherwise = 0
  where discriminante = b^2 - 4 * a * c

-- Ejercicio 9: raices
raices a b c
  | discriminante < 0 = []
  | discriminante == 0 = [-b / (2 * a)]
  | otherwise = [(-b + sqrt discriminante) / (2 * a), (-b - sqrt discriminante) / (2 * a)]
  where
    discriminante = b^2 - 4 * a * c

-- Ejercicio 10: area
area a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where s = (a + b + c) / 2

-- Ejercicio 11: interseccion
interseccion [] _ = []
interseccion _ [] = []
interseccion [a,b] [c,d]
  | max a c <= min b d = [max a c, min b d]
  | otherwise = []

-- Ejercicio 12: linea
linea n = [start .. start + n - 1]
  where start = n * (n - 1) `div` 2 + 1

-- Definición del tipo de datos Estudiante
data Estudiante = Estudiante {
  nombre :: String,
  apellido :: String,
  edad :: Int,
  numeroControl :: String
} deriving (Show)

-- Lista Estudiantes
listaEstudiantes = [
  Estudiante "Carlos" "Perez" 20 "20210001",
  Estudiante "Ana" "Lopez" 22 "20200001",
  Estudiante "Juan" "Gomez" 19 "20220003",
  Estudiante "Maria" "Martinez" 21 "20210004",
  Estudiante "Pedro" "Hernandez" 20 "20200005",
  Estudiante "Sofia" "Diaz" 23 "20200006",
  Estudiante "Luis" "Ramirez" 22 "20200007",
  Estudiante "Laura" "Garcia" 19 "20220008",
  Estudiante "Fernando" "Sanchez" 24 "20190009",
  Estudiante "Lucia" "Ortiz" 20 "20210010"
  ]

ordenarPorEdad [] = []
ordenarPorEdad (x:xs) = ordenarPorEdad menores ++ [x] ++ ordenarPorEdad mayores
  where
    menores = [y | y <- xs, edad y <= edad x]
    mayores = [y | y <- xs, edad y > edad x]

menorEstudiante [x] = x
menorEstudiante (x:xs) =
  let menorResto = menorEstudiante xs
  in if edad x <= edad menorResto then x else menorResto

mayorEstudiante [x] = x
mayorEstudiante (x:xs) =
  let mayorResto = mayorEstudiante xs
  in if edad x >= edad mayorResto then x else mayorResto

promedioEdades estudiantes = sumaEdades estudiantes / fromIntegral (contarElementos estudiantes)

sumaEdades [] = 0
sumaEdades (x:xs) = fromIntegral (edad x) + sumaEdades xs

contarElementos [] = 0
contarElementos (_:xs) = 1 + contarElementos xs

data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) deriving (Show)

insertar x Vacio = Nodo x Vacio Vacio
insertar x (Nodo y izq der)
  | x < y     = Nodo y (insertar x izq) der
  | otherwise = Nodo y izq (insertar x der)

insertarDesdeArreglo [] = Vacio
insertarDesdeArreglo (x:xs) = insertar x (insertarDesdeArreglo xs)

buscar _ Vacio = False
buscar x (Nodo y izq der)
  | x == y    = True
  | x < y     = buscar x izq
  | otherwise = buscar x der

inorden Vacio = []
inorden (Nodo x izq der) = inorden izq ++ [x] ++ inorden der

posorden Vacio = []
posorden (Nodo x izq der) = posorden izq ++ posorden der ++ [x]

preorden Vacio = []
preorden (Nodo x izq der) = [x] ++ preorden izq ++ preorden der