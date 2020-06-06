module Lib where
import Text.Show.Functions
import Data.List (find)

laVerdad = True

{- De los ratones nos interesa modelar su nombre, su edad (en años), su peso, y las enfermedades que posee.
Por ejemplo:
Cerebro es un ratón con 9 años, 0.2 kg de peso y tiene brucelosis, sarampión y tuberculosis.
Bicenterrata es un ratón con 256 años, 0.2kg de peso, y completamente sano.
Huesudo es un ratón de 4 años con 10kg de peso, y alta obesidad y sinusitis.
-}


-- PUNTO1. Modelar a los ratones mencionados.

data Raton = UnRaton {
    edad    :: Float,
    peso    :: Float,
    enfermedades :: [Enfermedad]
} deriving (Eq, Show)

type Enfermedad = String


cerebro = UnRaton { edad = 9, peso = 0.2, enfermedades = ["brucelosis", "sarampion", "tuberculosis"]}
bicenterrata = UnRaton { edad = 256, peso = 0.2, enfermedades = []}
huesudo = UnRaton { edad = 4, peso = 10, enfermedades = ["obesidad", "sinusitis"]}


-- PUNTO 2
type Hierba = Raton -> Raton
{-hierbaBuena, que rejuvenece al ratón a la raíz cuadrada de su edad.
Por ejemplo, si a cerebro le doy hierbaBuena, se transforma en un ratón de 3 años.
-}

hierbaBuena :: Hierba
hierbaBuena raton = raton { edad = modificarEdad (edad raton)}

modificarEdad :: Float -> Float
modificarEdad edad = sqrt edad

{- hierbaVerde, elimina las enfermedades que terminen de cierta forma.
Por ejemplo, si a cerebro le doy la hierbaVerde del tipo “sis”, queda sólo con sarampión.-}

hierbaVerde :: String -> Hierba
hierbaVerde terminacion raton = raton { enfermedades = modificarEnfermedades terminacion (enfermedades raton)}

modificarEnfermedades :: String -> [Enfermedad] -> [Enfermedad]
modificarEnfermedades _ [] = []
modificarEnfermedades terminacion enfermedades = filter (not.verificarTerminacion terminacion) enfermedades

verificarTerminacion :: String -> Enfermedad -> Bool                
verificarTerminacion terminacion enfermedad = all (&& True) (zipWith (==) (reverse terminacion) (reverse enfermedad))

{-alcachofa, hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde un 5%.
Por ejemplo, un raton de 3 kg queda con 2,7 kg y cerebro queda con 0.19 kg. -}

alcachofa :: Hierba
alcachofa raton = raton { peso = disminucionPorcPeso (peso raton)}

disminucionPorcPeso peso
    | peso > 2 = peso - (peso * 0.1)
    | otherwise = peso - (peso * 0.05)

{-hierbaZort, hace que el ratón se transforme en Pinky, perdiendo todas sus enfermedades y quedando con 0 años de edad.-}
hierbaZort :: Hierba
hierbaZort raton = transformarEnPinky raton

transformarEnPinky :: Hierba
transformarEnPinky raton = raton { edad = 0, enfermedades = []} 

--hierbaDelDiablo, hace que el ratón pierda 0.1kg (sin disminuir de 0) y elimina todas las enfermedades con menos de 10 letras
hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = raton { peso = disminucionKgPeso (peso raton), enfermedades = enfermedadMenor10Letras (enfermedades raton)}

disminucionKgPeso :: Float -> Float
disminucionKgPeso peso
    | peso - 0.1 < 0 = 0
    | otherwise = peso -0.1

enfermedadMenor10Letras :: [Enfermedad] -> [Enfermedad]
enfermedadMenor10Letras enfermedades = filter ((>9).length) enfermedades


-- PUNTO 3
type CriterioMedicamento = Raton -> Raton

aplicarMedicamento :: CriterioMedicamento -> Raton -> Raton
aplicarMedicamento medicamento raton = medicamento raton


prepararHierba :: Hierba -> Raton -> Raton
prepararHierba hierba raton =  hierba raton

prepararHierba2 :: Hierba -> Int -> Raton -> Raton
prepararHierba2 hierba n = (!! n). take (n+1). iterate (prepararHierba hierba) 

{- Hacer el pondsAntiAge, que es un medicamento que está hecho con 3 hierbas buenas y una alcachofa. iterate
Por ejemplo, si se lo administramos al ratón Bicenterrata, queda con 2 años y 0.19 kg -}

pondsAntiAge :: CriterioMedicamento
pondsAntiAge = prepararHierba alcachofa . prepararHierba2 hierbaBuena 3 

{-Hacer el reduceFatFast, (que viene en distintas potencias) y es un medicamento compuesto por una hierbaVerde
 de “obesidad” y tantas alcachofas como indique su potencia.
Por ejemplo administrándole a Huesudo un reduceFatFast de potencia 1 hace que huesudo pase a pesar 9 kg y sólo quede con sinusitis. 
Si en lugar de la 1 le administramos un reduceFatFast de potencia 2, pasa a pesar 8.1 kg y queda también solo con sinusitis.-}

reduceFast :: Int -> CriterioMedicamento
reduceFast n = prepararHierba (hierbaVerde "obesidad") . prepararHierba2 alcachofa n  

{-Hacer la pdepCilina, que es un medicamento que usa hierbasVerdes para curar todas las enfermedades infecciosas. 
Las enfermedades infecciosas son aquellas cuyo nombre termina de alguna de estas formas (utilizar esta constante):
-}

sufijosInfecciosas = ["sis", "itis", "emia", "cocos"]

pdepCilina :: [String] -> CriterioMedicamento
pdepCilina sufijosInfecciosas raton = foldl funcion raton sufijosInfecciosas
    where funcion = flip hierbaVerde


-- PUNTO 4
{-Hacer la función que encuentra la cantidadIdeal. Recibe una condición y dice cuál es el primer número natural que la cumple.
-}

-- **************************--
--              A
-- **************************--
cantidadIdeal :: (a -> Bool) -> [a] -> Maybe a
cantidadIdeal condicion lista = find condicion lista

{-Saber si un medicamento lograEstabilizar una comunidad de ratones. Esto sucede cuando, luego de aplicarle el medicamento a todos 
los ratones de la comunidad, se elimina el sobrepeso y todos tienen menos de 3 enfermedades. Un ratón tiene sobrepeso si pesa más de 1kg.-}

-- **************************--
--              B
-- **************************--
type Comunidad = [Raton]

medicamentoEstabilizador :: CriterioMedicamento -> Comunidad -> Bool
medicamentoEstabilizador medicamento ratones = (filtroMasDe3Enfermedades (ratonesMedicados medicamento ratones)) && 
    (filtroSobrepeso  (ratonesMedicados medicamento ratones))


ratonesMedicados :: CriterioMedicamento -> Comunidad -> Comunidad
ratonesMedicados medicamento ratones = map medicamento ratones

filtroSobrepeso:: Comunidad -> Bool
filtroSobrepeso = all (not.tieneSobrepeso) . (map peso)

filtroMasDe3Enfermedades :: Comunidad -> Bool
filtroMasDe3Enfermedades = all ((< 3).length). (map enfermedades)

tieneSobrepeso :: Float -> Bool
tieneSobrepeso peso = peso > 1


{-Diseñar el siguiente experimento: dado una comunidad de ratones, encontrar la potencia ideal del reduceFatFast 
necesaria para estabilizar la comunidad.-}

-- **************************--
--              C
-- **************************--
potenciaIdeal :: Comunidad -> Maybe Int
potenciaIdeal ratones = cantidadIdeal (/= 0) (ratonesEstabilizados ratones 1)

ratonesEstabilizados :: [Raton] -> Int -> [Int]
ratonesEstabilizados ratones n = condicion ratones n : ratonesEstabilizados nuevosRatones (n+1)
    where nuevosRatones = ratonesMedicados (reduceFast (n-1)) ratones

condicion :: [Raton] -> Int -> Int
condicion ratones n
    | medicamentoEstabilizador (reduceFast n) ratones = n
    | otherwise = 0
