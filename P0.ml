(*Implementación de la función mapdoble*)
let rec mapdoble f1 f2 l = match l with
  [] -> []
  | h1::h2::t -> f1 h1::f2 h2::mapdoble f1 f2 t
  | h1::[] -> f1 h1::[];;

(*Tipo de la función mapdoble*)
(*val mapdoble : ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list = <fun>*)

(*Valor de: 
mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];;
Error: This expression has type string but an expression was expected of type int
mapdoble espera que ambas funciones devuelvan el mismo tipo de dato, que ha de ser el tipo 
de dato de toda la lista*)

(*Tipo de *)
let y = function x -> 5 in mapdoble y;;
(*- : ('_weak1 -> int) -> '_weak1 list -> int list = <fun>
Estamos indicando que el primer argumento que recibe mapdoble es la función x -> 5
por lo que el tipo de "mapdoble y" es una función parcialmente aplicada. De ahí los weak*)
