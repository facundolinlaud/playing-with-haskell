import TP

--funcion que corre todos los tests
runTests :: IO ()
runTests = putStr (foldr (++) "" [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,test21,test22,test23,test24])

-- Dado un resultado valor de verdad devuelve un string mas declarativo
mostrarResultado :: Bool -> String
mostrarResultado True = "OK"
mostrarResultado False = "FALLO"

--Todos los tests tienen el mismo tipo
-- testX :: String
-- compara iniciar de una cadena de ADN y la compara con el resultado esperado. Esa comparacion se muestra de una forma mas declarativa de la pinta:
-- TestX : OK 		o bien 		TestX : Fallo
test1  = "Test1  :" ++ (mostrarResultado (iniciar [A,T,A,C,T,C,G,T,A,A,T,T,C,A,C,T,C,C] == [[Ser,Ile,Lys]])) ++ "\n"
test2  = "Test2  :" ++ (mostrarResultado (iniciar [C,A,C,C,T,G,T,A,C,A,C,A,G,A,G,G,T,A,A,C,T,T,A,G] == [[Cys,Leu,His]])) ++ "\n"
test3  = "Test3  :" ++ (mostrarResultado (iniciar [T,T,A,A,T,A,C,G,A,C,A,T,A,A,T,T,A,T] == [[Leu,Tyr],[Ser,Tyr]])) ++ "\n"
test4  = "Test4  :" ++ (mostrarResultado (iniciar [G,C,C,T,T,G,A,T,A,T,G,G,A,G,A,A,C,T,C,A,T,T,A,G,A,T,A] == [])) ++ "\n"
test5  = "Test5  :" ++ (mostrarResultado (iniciar [A,A,G,T,G,T,A,T,G,T,T,G,A,A,T,T,A,T,A,T,A,A,A,A,C,G,G,G,C,A,T,G,A] == [[Leu,Asn,Tyr,Ile,Lys,Arg,Ala]])) ++ "\n"
test6  = "Test6  :" ++ (mostrarResultado (iniciar [A,T,G,A,T,G,A,T,G,G,C,T,T,G,A] == [[Met,Met,Ala],[Met,Ala],[Ala]])) ++ "\n"
test7  = "Test7  :" ++ (mostrarResultado (iniciar [A,T,G,A,T,T,T,A,G,T,T,A,T,G,T,A,T,T,G,A] == [[Ile],[Tyr],[Tyr]])) ++ "\n"
test8  = "Test8  :" ++ (mostrarResultado (iniciar [A,T,G,A,T,G,A] == [])) ++ "\n"
test9  = "Test9  :" ++ (mostrarResultado (iniciar [A,T,G,T,G,A] == [])) ++ "\n"
test10 = "Test10 :" ++ (mostrarResultado (iniciar [A,T,G,T,G,A,A,T,G,T,T,T,T,G,A] == [[Phe]])) ++ "\n"
test11 = "Test11 :" ++ (mostrarResultado (iniciar [A,G,T,T,T,T,G,T,A,A,G,T,G,T,A] == [[Phe]])) ++ "\n"
test12 = "Test12 :" ++ (mostrarResultado (iniciar [T,A,C,A,C,T,T,A,C,A,A,A,A,C,T] == [[Phe]])) ++ "\n"
test13 = "Test13 :" ++ (mostrarResultado (iniciar [T,C,A,A,A,A,C,A,T,T,C,A,C,A,T] == [[Phe]])) ++ "\n"
test14 = "Test14 :" ++ (mostrarResultado (iniciar [A,T,T,T,T,G,A,G,A,T,T,A,A,G,T,A,A,T,A,G,G,A,A,G,T,C,C,C,C,A,T,A,G,A] == [[Asn]])) ++ "\n"
test15 = "Test15 :" ++ (mostrarResultado (iniciar [A,T,G,A,T,G,A,T,G,A,T,G,T,A,A] == [[Met,Met,Met],[Met,Met],[Met]])) ++ "\n"
test16 = "Test16 :" ++ (mostrarResultado (iniciar [A,T,G,T,C,G,A,T,C,A,G,C,A,T,C,G,A,C,T,A,C,G,A,C,T,A,C,G,A,C,G,A,C,T,A,C,A,G,C,A,T,C,A,G,T,C,A,G,T,C,A,T,C,A,G,C,A,T,C,A,G,T,A,C,C,T,A,C,T,A,C,T,C,A,A,A,C,A,C,C,A,C,A,G,A,T,A,T,T,T,G,A,G,A,T,T,A,A,G,T,A,A,T,A,G,G,A,A,G,T,C,C,C,C,A,T,A,G,A] == [[Leu,Met,Leu,Leu,Met,Ser],[Leu,Leu,Met,Ser],[Ser],[Met,Ser,Leu,Trp,Cys,Leu],[Ser,Leu,Trp,Cys,Leu],[Gly,Thr,Ser,Tyr,Tyr,Leu,Ile,Ser,Asn,Ile,Cys,Gly,Val],[Leu,Met,Thr,Asp],[Thr,Asp],[Leu],[Ser,Ile,Ser,Ile,Asp,Tyr,Asp,Tyr,Asp,Asp,Tyr,Ser,Ile,Ser,Gln,Ser,Ser,Ala,Ser,Val,Pro,Thr,Thr,Gln,Thr,Pro,Gln,Ile,Phe,Glu,Ile,Lys],[Asn],[Thr,Thr,Thr,Thr,Asp]])) ++ "\n"
test17 = "Test17 :" ++ (mostrarResultado (iniciar [A,A,T,C,G,T,C,A,A,T,C,G,T,C,A,G,T,C,G,A,T,A,T,A,T,A,G,C,G,C,G,C,T,C,T,A,T,T,G] == [])) ++ "\n"
test18 = "Test18 :" ++ (mostrarResultado (iniciar [A,T,G,G,G,T,C,A,G,C,T,G,T,T,C,T,C,T,T,C,A,C,C,T,A,A,G,A,G,T,G,A,T,G,A,G,A,A,T,A,A,T,G,A,T,T,T,G,C,C,C,T,C,C,A,G,C,T,T,T,A,C,T,G,G,T,T,A,T,T,T,T,A,A,G,A,A,A,T,T,T,A,A,T,A,C,G,G,G,A,A] == [[Thr,Asn,Lys,Ile,Leu]])) ++ "\n"
test19 = "Test18 :" ++ (mostrarResultado (iniciar [G,A,A,A,A,A,T,C,A,T,T,T,C,T,C,A,A,G,A,G,A,T,C,C,T,C,A,A,T,T] == [])) ++ "\n"
test20 = "Test20 :" ++ (mostrarResultado (iniciar [T,G,A,T,T,G,A,A,T,T,A,A,G,G,A,T,G,A,G,A,A,A,A,G,G,G,A,A,T,A,T,T,C,A,G,T,T,G,A,C,A,A,A,C,T,C,T,G,C,A,A,T,C,A,G,T,G,A,T] == [])) ++ "\n"
test21 = "Test21 :" ++ (mostrarResultado (iniciar [G,C,A,T,T,A,A,A,A,G,A,A,A,T,C,G,A,T,A,G,T,A,G,T,G,T,G,C,T,C,A,A,T,G,T,T,G,C,T,G,T,C,A,C,C,G,G,G,G,A,G,A,C,G,G,G,A,T,C,A,G,G,G,A,A,G,T,C,C,A,G,C,T,T,C,A,T,C,A,A,T,A,C,C,C,T,G,A,G,A,G,G,C,A,T,T,G,G,G,A] == [[Leu,Leu,Ser,Pro,Gly,Arg,Arg,Asp,Gln,Gly,Ser,Pro,Ala,Ser,Ser,Ile,Pro]])) ++ "\n"
test22 = "Test22 :" ++ (mostrarResultado (iniciar [A,T,G,A,A,G,A,A,G,A,A,G,G,T,G,C,A,G,C,T,A,A,A,A,C,T,G,G,G,G,T,G,G,T,G,G,A,G,G,T,A,A,C,C,A,T,G,G,A,A,A,G,A,C,A,T,C,C,A,T,A,C,A,A,A,C,A,C,C,C,C,A,A,T,A,T,A,C,C,C] == [[Asp,Val,Phe,Pro,Trp,Leu,Pro,Pro,Pro,Pro,Gln,Phe],[Lys,Lys,Lys,Val,Gln,Leu,Lys,Leu,Gly,Trp,Trp,Arg]])) ++ "\n"
test23 = "Test23 :" ++ (mostrarResultado (iniciar [A,A,T,G,T,G,G,T,T,T,T,T,T,G,G,G,A,C,C,T,G,C,C,T,G,G,G,A,T,T,G,G,A,A,G] == [])) ++ "\n"
test24 = "Test24 :" ++ (mostrarResultado (iniciar [C,A,C,A,A,A,T,T,T,C,C,C,A,C,C,A,A,A,C,A,C,T,T,A,C,C,T,G,G,A,G,A,A,A,A,T,G,A,A,G,T,T,C,T,A,T,G,A,G,T,A,C,G,A,T,T,T,C,T,T,C,A,T,T,A,T,T,A,T,T,T,C] == [[Asp,Leu,Phe,Tyr,Phe,Lys,Ile,Leu,Met,Leu,Lys,Lys],[Leu,Lys,Lys],[Lys,Lys,Ser,Tyr,Ser],[Ser,Ile,Leu,Lys]])) ++ "\n"
