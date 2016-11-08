import Test.HUnit

import Base
import ManejoDeBases
import SintesisDeProteinas
import TP

testComplementarBase1 :: Test
testComplementarBase1 = TestCase (assertEqual "for complementarBase1" A (complementarBase T))

testComplementarBase2 :: Test
testComplementarBase2 = TestCase (assertEqual "for complementarBase2" C (complementarBase G))

testComplementarCadenaDNA1 :: Test
testComplementarCadenaDNA1 = TestCase (assertEqual "for complementarCadenaDNA" 
        [A, C, G, T, T, G, A, C] 
        (complementarCadenaDNA [T, G, C, A, A, C, T, G]))

testComplementarCadenaDNA2 :: Test
testComplementarCadenaDNA2 = TestCase (assertEqual "for complementarCadenaDNA" 
        [A, T, G, A, G, C, A, T, T, A, A, G, T, G, A] 
        (complementarCadenaDNA [T, A, C, T, C, G, T, A, A, T, T, C, A, C, T]))

testObtenerCadenaReverseDNA1 :: Test
testObtenerCadenaReverseDNA1 = TestCase (assertEqual "for obtenerCadenaReverseDNA" 
        [A, C, G, T, T, G, A, C] 
        (obtenerCadenaReverseDNA [C, A, G, T, T, G, C, A]))

testTranscribir1 :: Test
testTranscribir1 = TestCase (assertEqual "for transcribir" 
        [A, U, G, A, G, C, A, U, U, A, A, G, U, G, A] 
        (transcribir [T, A, C, T, C, G, T, A, A, T, T, C, A, C, T]))

testTranscribir2 :: Test
testTranscribir2 = TestCase (assertEqual "for transcribir" 
        [U, G, C, A, A, C, U, G] 
        (transcribir [A, C, G, T, T, G, A, C]))

testQuitarCadenasVacias1 :: Test
testQuitarCadenasVacias1 = TestCase (assertEqual "for quitarCadenasVacias" 
        [[Lys], [Lys, Lys]] 
        (quitarCadenasVacias [[], [Lys], [], [Lys, Lys], []]))

testQuitarCadenasVacias2 :: Test
testQuitarCadenasVacias2 = TestCase (assertEqual "for quitarCadenasVacias" 
        [] 
        (quitarCadenasVacias []))

testEncontrarInicios1 :: Test
testEncontrarInicios1 = TestCase (assertEqual "for encontrarInicios1"
        [[A, G, C, A, U, U, A, U, G, A, A, G, U, G, A, G, G, A, U, G], [A, A, G, U, G, A, G, G, A, U, G]]
        (encontrarInicios [U, A, U, G, A, G, C, A, U, U, A, U, G, A, A, G, U, G, A, G, G, A, U, G]))

testCortarSobrante1 :: Test
testCortarSobrante1 = TestCase (assertEqual "for cortarSobrante"
        [[A, G, C, A, U, A, G, C, A, U, A, G], [G, C, U, U, A, G, C, A, U], [A, G, C, U, G, C]]
        (cortarSobrante [[A, G, C, A, U, A, G, C, A, U, A, G, C], [G, C, U, U, A, G, C, A, U, U, C], [A, G, C, U, G, C]]))

testEncontrarFinal1 :: Test
testEncontrarFinal1 = TestCase (assertEqual "for encontrarFinal"
        [[A, G, C, A, U, A], [G, C, U], [A, G, C]]
        (encontrarFinal [[A, G, C, A, U, A, U, G, A, U, A, G], [G, C, U, U, A, G, C, A, U], [A, G, C, U, A, A]]))

testEncontrarFinal2 :: Test
testEncontrarFinal2 = TestCase (assertEqual "for encontrarFinal"
        []
        (encontrarFinal [[A, G, C, A, U, A, U, A, C]]))

testEncontrarFinal3 :: Test
testEncontrarFinal3 = TestCase (assertEqual "for encontrarFinal"
        []
        (encontrarFinal [[U, A, G, A, C, G]]))

tests :: Test
tests = TestList [TestLabel "testComplementarBase1" testComplementarBase1,
                  TestLabel "testComplementarBase2" testComplementarBase2,
                  TestLabel "testComplementarCadenaDNA1" testComplementarCadenaDNA1,
                  TestLabel "testComplementarCadenaDNA2" testComplementarCadenaDNA2,
                  TestLabel "testObtenerCadenaReverseDNA1" testObtenerCadenaReverseDNA1,
                  TestLabel "testTranscribir1" testTranscribir1,
                  TestLabel "testTranscribir2" testTranscribir2,
                  TestLabel "testQuitarCadenasVacias1" testQuitarCadenasVacias1,
                  TestLabel "testQuitarCadenasVacias2" testQuitarCadenasVacias2,
                  TestLabel "testEncontrarInicios1" testEncontrarInicios1,
                  TestLabel "testCortarSobrante1" testCortarSobrante1,
                  TestLabel "testEncontrarFinal1" testEncontrarFinal1,
                  TestLabel "testEncontrarFinal2" testEncontrarFinal2,
                  TestLabel "testEncontrarFinal3" testEncontrarFinal3]