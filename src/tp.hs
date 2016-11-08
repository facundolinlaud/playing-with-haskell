module TP where

import Base

complementarBase :: BaseNucleotidica -> BaseNucleotidica
complementarBase A = T
complementarBase T = A
complementarBase C = G
complementarBase G = C
complementarBase b = b

complementarCadenaDNA :: CadenaDNA -> CadenaDNA
complementarCadenaDNA [] = []
complementarCadenaDNA (x : xs) = complementarBase x : complementarCadenaDNA xs

obtenerCadenaReverseDNA :: CadenaDNA -> CadenaDNA
obtenerCadenaReverseDNA [] = []
obtenerCadenaReverseDNA (x : xs) = (obtenerCadenaReverseDNA xs) ++ [x]

transcribir :: CadenaDNA -> CadenaRNA
transcribir [] = []
transcribir (A : xs) = U : transcribir xs
transcribir (x : xs) = complementarBase x : transcribir xs

iniciar :: CadenaDNA -> [Proteina]
iniciar x = quitarCadenasVacias(
                iniciarAux [transcribir x,
                            transcribir(reverse x),
                            transcribir(complementarCadenaDNA x),
                            transcribir(complementarCadenaDNA(reverse x))])

quitarCadenasVacias :: [Proteina] -> [Proteina]
quitarCadenasVacias [] = []
quitarCadenasVacias ([] : xs) = quitarCadenasVacias xs
quitarCadenasVacias (x : xs) = x : quitarCadenasVacias xs

iniciarAux :: [CadenaRNA] -> [Proteina]
iniciarAux [] = []
iniciarAux (x : xs) = obtenerProteinas(
                        encontrarFinal(
                            cortarSobrante(
                                 encontrarInicios x))) ++ iniciarAux xs

encontrarInicios :: CadenaRNA -> [CadenaRNA]
encontrarInicios [] = []
encontrarInicios (_ : []) = []
encontrarInicios (_ : _ : []) = []
encontrarInicios (_ : _ : _ : []) = []
encontrarInicios (A : U : G : xs) = [xs] ++ encontrarInicios xs
encontrarInicios (_ : xs) = encontrarInicios xs

cortarSobrante :: [CadenaRNA] -> [CadenaRNA]
cortarSobrante [] = []
cortarSobrante (x : xs) = cortarSobranteAux x : cortarSobrante xs

cortarSobranteAux :: CadenaRNA -> CadenaRNA
cortarSobranteAux [] = []
cortarSobranteAux (_ : []) = []
cortarSobranteAux (_ : _ : []) = []
cortarSobranteAux (a : b : c : xs) = a : b : c : cortarSobranteAux xs

encontrarFinal :: [CadenaRNA] -> [CadenaRNA]
encontrarFinal [] = []
encontrarFinal (x : xs) | length(baseSinFinal) == length x = encontrarFinal xs
                        | length(baseSinFinal) == 0 = encontrarFinal xs
                        | otherwise = baseSinFinal : encontrarFinal xs
                          where baseSinFinal = encontrarFinalAux x

encontrarFinalAux :: CadenaRNA -> CadenaRNA
encontrarFinalAux [] = []
encontrarFinalAux (U : A : A : xs) = []
encontrarFinalAux (U : A : G : xs) = []
encontrarFinalAux (U : G : A : xs) = []
encontrarFinalAux (a : b : c : xs) = a : b : c : encontrarFinalAux xs

obtenerProteinas :: [CadenaRNA] -> [Proteina]
obtenerProteinas [] = []
obtenerProteinas (x : xs) = obtenerProteinasAux x : obtenerProteinas xs

obtenerProteinasAux :: CadenaRNA -> Proteina
obtenerProteinasAux [] = []
obtenerProteinasAux (a : b : c : xs) = (traducirCodonAAminoacido(a, b, c)) : obtenerProteinasAux xs