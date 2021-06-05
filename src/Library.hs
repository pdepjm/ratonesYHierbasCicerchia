module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Raton = UnRaton {
    nombre :: String,
    edad :: Number,
    peso :: Number,
    enfermedades :: [String]
}deriving(Show,Eq)

--PUNTO 1

cerebro = UnRaton {
    nombre = "Cerebro",
    edad = 9,
    peso = 0.2,
    enfermedades = ["brucelosis","sarampion","tuberculosis"]
}


bicenterrata = UnRaton {
    nombre = "Bicenterrata",
    edad = 256,
    peso = 0.2,
    enfermedades = []
}

huesudo = UnRaton {
    nombre = "Huesudo",
    edad = 4,
    peso = 10,
    enfermedades = ["alta obesidad","sinusitis"]
}

--PUNTO 2

type Hierba = Raton -> Raton

--a

hierbaBuena :: Hierba
hierbaBuena raton = raton {edad = sqrt (edad raton)}

--b

hierbaVerde :: String -> Hierba
hierbaVerde terminacion raton = raton {enfermedades = filter (terminaCon terminacion) (enfermedades raton)}

terminaCon :: String -> String -> Bool
terminaCon terminacion enfermedad = reverse ((take (length terminacion) (reverse enfermedad))) /= terminacion

--c

alcachofa :: Hierba
alcachofa raton | peso raton > 2 = raton {peso = peso raton * 0.9}
                | otherwise = raton {peso = peso raton * 0.95}

--d

hierbaZort :: Hierba
hierbaZort raton = raton {nombre = "Pinky",edad = 0,enfermedades=[]}

--e

hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = raton {peso = max (peso raton - 0.1) 0,enfermedades = eliminaEnfermedadesConMenosDeDiezLetras (enfermedades raton)}

eliminaEnfermedadesConMenosDeDiezLetras :: [String] -> [String]
eliminaEnfermedadesConMenosDeDiezLetras enfermedades = filter ((>=10).length) enfermedades

--PUNTO 3

type Medicamento = [Hierba]

administrarMedicamento :: Raton -> Medicamento -> Raton
administrarMedicamento raton medicamentos = foldl administrarHierba raton medicamentos

administrarHierba :: Raton -> Hierba -> Raton
administrarHierba raton hierba = hierba raton

--a

pondsAntiAge :: Medicamento
pondsAntiAge = [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa]

--b

type Potencia = Number
reduceFatFast :: Potencia -> Medicamento
reduceFatFast potencia = [hierbaVerde "obesidad"] ++ replicate potencia alcachofa

--c

sufijosInfecciosas = ["sis", "itis", "emia", "cocos"]

pdepCilina :: Medicamento
pdepCilina = map hierbaVerde sufijosInfecciosas

--PUNTO 4

--a
type Condicion = (Number -> Bool)

cantidadIdeal :: Condicion -> Number
cantidadIdeal condicion = head (filter condicion numerosInfinitos)

numerosInfinitos = iterate (+1) 1

--b

lograEstabilizar :: Medicamento -> [Raton] -> Bool
lograEstabilizar medicamento ratones = all (estaEstabilizado medicamento) ratones

estaEstabilizado :: Medicamento -> Raton -> Bool
estaEstabilizado medicamento raton = sinSobrePeso (administrarMedicamento raton medicamento) && menosDeTresEnfermedades (administrarMedicamento raton medicamento)

sinSobrePeso :: Raton -> Bool
sinSobrePeso = (<1).peso

menosDeTresEnfermedades :: Raton -> Bool
menosDeTresEnfermedades = (<3).length.enfermedades

--c

potenciaIdeal :: [Raton] -> Number
potenciaIdeal = encontrarPotenciaIdeal 1

encontrarPotenciaIdeal :: Number -> [Raton] -> Number
encontrarPotenciaIdeal potencia ratones
                | lograEstabilizar (reduceFatFast potencia) ratones = potencia
                | otherwise = 1 + encontrarPotenciaIdeal (potencia+1) ratones
