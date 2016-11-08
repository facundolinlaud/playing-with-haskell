module TP where

import Base
import ManejoDeBases
import SintesisDeProteinas

iniciar :: CadenaDNA -> [Proteina]
iniciar x = quitarCadenasVacias(
                sintetizarProteinas [transcribir x,
                                     transcribir(obtenerCadenaReverseDNA x),
                                     transcribir(complementarCadenaDNA x),
                                     transcribir(obtenerCadenaReverseDNA (complementarCadenaDNA x))])

quitarCadenasVacias :: [Proteina] -> [Proteina]
quitarCadenasVacias [] = []
quitarCadenasVacias ([] : xs) = quitarCadenasVacias xs
quitarCadenasVacias (x : xs) = x : quitarCadenasVacias xs