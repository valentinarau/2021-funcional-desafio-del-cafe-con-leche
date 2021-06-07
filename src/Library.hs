module Library where
import PdePreludat hiding (foldr, sum, elem, all)
import Prelude (foldr, sum, elem, all, Foldable(..))

-- Desafio 1: Look and say, o https://es.wikipedia.org/wiki/Constante_de_Conway
--
-- Construir la siguiente sucesión:
-- 1
-- 11
-- 21
-- 1211
-- 111221
-- 312211
-- etc...
-- 

lookAndSay :: [Number]
lookAndSay = 1:map(lookAndSay'n [1]) [1,2..]

lookAndSay'n :: [Number] -> Number -> Number
lookAndSay'n lista 0  = unirNumeros lista
lookAndSay'n lista n  = lookAndSay'n (listarRepetidos lista) (n-1)

listarRepetidos :: [Number]->[Number]
listarRepetidos [] = []
listarRepetidos lista = numeroSeRepite (nVeces lista) lista ++ listarRepetidos ( notTake (nVeces lista) lista)

nVeces :: [Number]->Number
nVeces [x] = 1
nVeces (x:y:resto)
  |x == y = 1 + nVeces (y:resto)
  |otherwise = 1

numeroSeRepite :: Number->[Number]->[Number]
numeroSeRepite n lista = n:take 1 lista

unirNumeros :: [Number] -> Number
unirNumeros [x] = x
unirNumeros (x:y:resto) = unirNumeros(x*10+y:resto)

notTake :: Number -> [Number] -> [Number]
notTake 0 lista = lista
notTake _ [] = []
notTake n (x:resto) = notTake (n-1) resto

--------------------------------------------------------------------

-- Desafio 2: Un foldr para una flor
--
-- En computación, un "rose tree" es un árbol donde cada nodo puede tener una
-- cantidad ilimitada de nodos hijos.

data RoseTree a = RoseTree a [RoseTree a]

-- Entonces, si quisiesemos representar un arbol como este:
--            "hola"
--           /   |    \
--       "soy" "el" "arbol"
-- lo podríamos escribir como:
holaSoyElArbol = RoseTree "hola" [RoseTree "soy" [],
                                  RoseTree "el" [],
                                  RoseTree "arbol" []]

-- y para mostrar otro mas complicado,
--             5
--          / / \ \  \
--         2 3  1 7   9
--        / \   |   / | \
--      10  11  4  0  6  20 
-- lo podríamos escribir como:
otroArbol = RoseTree 5 [RoseTree 2 [RoseTree 10 [],
                                    RoseTree 11 []],
                        RoseTree 3 [],
                        RoseTree 1 [RoseTree 4 []],
                        RoseTree 7 [],
                        RoseTree 9 [RoseTree 0 [],
                                    RoseTree 6 [],
                                    RoseTree 20 []]]

-- Algo que no vimos durante la cursada es que foldr en realidad es parte de una typeclass,
-- Foldable, o sea que no solo las listas se pueden foldear.
--
-- Como en el PdePreludat limitamos el tipo a que solo admita listas para hacerlo más simple,
-- en este ejercicio los imports están un poco tocados para traer el foldr que viene de haskell
-- en vez del del PdePreludat.
--
-- El desafío es hacer una implementación del foldr para esta estructura que haga pasar los tests.
--
-- Y algo interesante es que al implementar el foldr algunas funciones (como sum, all, any y elem)
-- las vamos a tener "gratis" para este tipo.
instance Foldable RoseTree where
foldr func seed (RoseTree a branches) = (func a.funcFoldr func  branches) seed

funcFoldr ::( a-> b -> b) -> [RoseTree a] -> b -> b
funcFoldr func [] = id
funcFoldr func [RoseTree a []] = func a
funcFoldr func (RoseTree b branch: branches) = func b.funcFoldr func branch.funcFoldr func branches

