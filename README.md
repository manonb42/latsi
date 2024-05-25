# latsi


Interpretor for LATSI using ocamlex and Menhir

# Execution 

dune build && dune exec src/main.exe < test


# Distribution du travail :
Baha Manon : Interpreteur + Extensions , Crague Ilian : Parseur


# Branches Extensions

## Extensions

Dans cette branche, on a ajouté diverses fonctionnalités. Les affectations peuvent être multiples (`a, b, c = 3, 4, 5`), ou peuvent être composées (`a += b`,  `a -= b`, `a *= b`, `a /= b`). Par soucis de lisibilité, on n'autorise pas les affectations à la fois multiples et composées. En revanche, on peut mettre plusieurs instructions sur la même ligne `a += 3; b += 5`. On peut aussi appeler des sousroutines `SOUSROUTINE 30`, et retourner de celles ci avec `RETOURNE`.

Les erreurs syntaxiques sont correctement rapportées.

## Extensions-2

Dans celle-ci, on reprend les extensions de `Extensions`, mais on enlève les numéros de ligne et on ajoute des boucles `TANT QUE ... { ... }` et `POUR .. DE .. JUSQUE ..`.

