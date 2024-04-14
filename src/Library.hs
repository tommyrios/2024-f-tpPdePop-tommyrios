module Library where
import PdePreludat

data Cancion = UnaCancion {
    nombre :: String,
    duracion :: Number,
    instrumentos :: [String]
} deriving (Show, Eq)

patternMatching :: Cancion
patternMatching = UnaCancion "Pattern Matching" 4 ["guitarra", "bajo", "batería"]

seisDieciocho :: Cancion
seisDieciocho = UnaCancion "Seis dieciocho" 3 ["teclado", "guitarra"]

laVidaEnHaskell :: Cancion
laVidaEnHaskell = UnaCancion "La vida en Haskell" 5 []

mariposaTekniTrueFalse :: Cancion
mariposaTekniTrueFalse = UnaCancion "Mariposa TekniTrueFalse" 6 ["guitarra", "bajo", "batería", "teclado"]

sigueCodeando :: Cancion
sigueCodeando = UnaCancion "Sigue Codeando" 3 ["guitarra", "bajo", "batería"]

aceptacion :: Cancion -> Number
aceptacion cancion
    | head (nombre cancion) == 'M' = 500
    | even (floor (duracion cancion)) = length (nombre cancion) * 10
    | null (instrumentos cancion) = 10
    | otherwise = 0

ordenRepertorio :: Cancion -> Cancion -> String
ordenRepertorio cancion1 cancion2
    | nombre cancion1 <= nombre cancion2 = nombre cancion1
    | otherwise = nombre cancion2

aceptada :: Cancion -> Bool
aceptada cancion 
    | aceptacion cancion > 60 = True 
    | otherwise = False

contieneInstrumento :: String -> Cancion -> Bool
contieneInstrumento instrumento cancion = elem instrumento (instrumentos cancion)

tocarCancion :: Cancion -> Number 
tocarCancion cancion
    | aceptada cancion = duracion cancion / 2
    | otherwise = duracion cancion

necesitaInstrumentos :: Cancion -> Bool
necesitaInstrumentos cancion
    | length (instrumentos cancion) > 0 = True
    | otherwise = False

esAcapella :: Cancion -> Bool
esAcapella cancion
    | necesitaInstrumentos cancion = False
    | otherwise = True
