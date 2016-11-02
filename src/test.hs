module Example where

import Test.HUnit
import Base

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

testRemoverListasVacias1 :: Test
testRemoverListasVacias1 = TestCase (assertEqual "for removerListasVacias" 
        [[Lys], [Lys, Lys]] 
        (removerListasVacias [[], [Lys], [], [Lys, Lys], []]))

testCortarInicio1 :: Test
testCortarInicio1 = TestCase (assertEqual "for cortarInicio"
        [A, G, C, A, U, U, A, A, G, U, G, A, G, G]
        (cortarInicio [U, A, U, G, A, G, C, A, U, U, A, A, G, U, G, A, G, G] False False False))

testCortarInicio2 :: Test
testCortarInicio2 = TestCase (assertEqual "for cortarInicio"
        []
        (cortarInicio [U, A, U, A, G, C, A, U, U, A, A, G, U, G, A, G, G] False False False))

testCortarFinal1 :: Test
testCortarFinal1 = TestCase (assertEqual "for cortarFinal"
        [A, G, C, A, U, U, A, A, G]
        (cortarFinal [A, G, C, A, U, U, A, A, G, U, G, A, G, G]))

testCortarFinal2 :: Test
testCortarFinal2 = TestCase (assertEqual "for cortarFinal"
        []
        (cortarFinal [A, G, C, A, U, U, A, A, G, G, G]))

testObtenerSubproteinas1 :: Test
testObtenerSubproteinas1 = TestCase (assertEqual "for obtenerSubproteinas"
        [Ser,Ile,Lys]
        (obtenerSubproteinas [A, G, C, A, U, U, A, A, G, U, G, A, G, G]))

testObtenerSubproteinas2 :: Test
testObtenerSubproteinas2 = TestCase (assertEqual "for obtenerSubproteinas"
        []
        (obtenerSubproteinas [A, G, C, A, U, U, A, A, G, U, G, G, G, G]))

testObtenerProteinas1 :: Test
testObtenerProteinas1 = TestCase (assertEqual "for obtenerProteinas"
        [[Ser,Ile,Lys]]
        (obtenerProteinas [A, T, A, C, T, C, G, T, A, A, T, T, C, A, C, T, C, C]))

testObtenerProteinas2 :: Test
testObtenerProteinas2 = TestCase (assertEqual "for obtenerProteinas"
        [[Leu,Tyr],[Ser,Tyr]]
        (obtenerProteinas [T, T, A, A, T, A, C, G, A, C, A, T, A, A, T, T, A, T]))

testObtenerProteinas3 :: Test
testObtenerProteinas3 = TestCase (assertEqual "for obtenerProteinas"
        []
        (obtenerProteinas [G, C, C, T, T, G, A, T, A, T, G, G, A, G, A, A, C, T, C, A, T, T]))

tests :: Test
tests = TestList [TestLabel "testComplementarBase1" testComplementarBase1,
                  TestLabel "testComplementarBase2" testComplementarBase2,
                  TestLabel "testComplementarCadenaDNA1" testComplementarCadenaDNA1,
                  TestLabel "testComplementarCadenaDNA2" testComplementarCadenaDNA2,
                  TestLabel "testObtenerCadenaReverseDNA1" testObtenerCadenaReverseDNA1,
                  TestLabel "testTranscribir1" testTranscribir1,
                  TestLabel "testTranscribir2" testTranscribir2,
                  TestLabel "testRemoverListasVacias1" testRemoverListasVacias1,
                  TestLabel "testCortarInicio1" testCortarInicio1,
                  TestLabel "testCortarInicio2" testCortarInicio2,
                  TestLabel "testCortarFinal1" testCortarFinal1,
                  TestLabel "testCortarFinal2" testCortarFinal2,
                  TestLabel "testObtenerSubproteinas1" testObtenerSubproteinas1,
                  TestLabel "testObtenerSubproteinas2" testObtenerSubproteinas2,
                  TestLabel "testObtenerProteinas1" testObtenerProteinas1,
                  TestLabel "testObtenerProteinas2" testObtenerProteinas2,
                  TestLabel "testObtenerProteinas3" testObtenerProteinas3]