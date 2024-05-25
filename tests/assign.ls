 10 a, b, c = 1, 3, 5
 20 IMPRIME "a, b, c = ", a, ", ", b,", ", c
 31 NL
 40 sum, prod = a+b+c, a*b*c
 50 IMPRIME "sum, prod = ", sum, ", ", prod
 61 NL
 70 a, b, c = c, b, a
 80 IMPRIME "swap ? a, b, c = ", a, ", ", b,", ", c, " -> "
 90 SI a = 5 ET b = 3 ET c = 1 ALORS IMPRIME "swap réussi"
 91 SAUF SI a = 5 ET b = 3 ET c = 1 ALORS IMPRIME "swap échoué"
101 NL
110 IMPRIME "a = 0 OU PAS b = 2 ET c = 1 "
111 SI a = 0 OU PAS b = 2 ET c = 1 ALORS IMPRIME "-> vrai" SINON IMPRIME "-> faux"
112 NL
120 IMPRIME "1 ? 2 : 3 -> ", 1 ? 2 : 3
121 NL
130 a = 0; b = 1; c = 2
140 IMPRIME "a, b, c = ", a, ", ", b,", ", c
141 NL
999 FIN
