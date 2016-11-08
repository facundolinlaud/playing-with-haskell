module ManejoDeBases where

import Base

obtenerCadenaReverseDNA :: CadenaDNA -> CadenaDNA
obtenerCadenaReverseDNA [] = []
obtenerCadenaReverseDNA (x : xs) = (obtenerCadenaReverseDNA xs) ++ [x]

complementarCadenaDNA :: CadenaDNA -> CadenaDNA
complementarCadenaDNA [] = []
complementarCadenaDNA (x : xs) = complementarBase x : complementarCadenaDNA xs

complementarBase :: BaseNucleotidica -> BaseNucleotidica
complementarBase A = T
complementarBase T = A
complementarBase C = G
complementarBase G = C
complementarBase b = b

transcribir :: CadenaDNA -> CadenaRNA
transcribir [] = []
transcribir (A : xs) = U : transcribir xs
transcribir (x : xs) = complementarBase x : transcribir xs