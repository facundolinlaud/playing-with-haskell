module SintesisDeProteinas where

import Base

sintetizarProteinas :: [CadenaRNA] -> [Proteina]
sintetizarProteinas [] = []
sintetizarProteinas (x : xs) = obtenerProteinas(
                                    encontrarFinal(
                                        cortarSobrante(
                                            encontrarInicios x))) ++ sintetizarProteinas xs

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