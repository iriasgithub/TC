(*EJERCICIO 1*)

(*Implementación de la función mapdoble*)
let rec mapdoble f1 f2 l = match l with
  [] -> []
  | h1::h2::t -> f1 h1::f2 h2::mapdoble f1 f2 t
  | h1::[] -> f1 h1::[]
;;

(*Tipo de la función mapdoble*)
(*val mapdoble : ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list = <fun>*)

(*Valor de: 
mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];;
Error: This expression has type string but an expression was expected of type int
mapdoble espera que ambas funciones devuelvan el mismo tipo de dato, que ha de ser el tipo 
de dato de toda la lista. En Ocaml las listas solo pueden contener un tipo*)

(*Tipo de *)
let y = function x -> 5 in mapdoble y;;
(*- : ('_weak1 -> int) -> '_weak1 list -> int list = <fun>
Estamos indicando que el primer argumento que recibe mapdoble es la función x -> 5
por lo que el tipo de "mapdoble y" es una función parcialmente aplicada. De ahí los weak*)


(*EJERCICIO 2*)

(*Implementación de la función primero_que_cumple*)
let rec primero_que_cumple f l = match l with 
  [] -> raise(Not_found)
  | h1::t -> if f h1 then h1 
             else primero_que_cumple f t
;;

(*Tipo de la función primero_que_cumple*)
(*val primero_que_cumple : ('a -> bool) -> 'a list -> 'a = <fun>*)

(*Implementación de la función existe*)
let existe f l =
  try
    let _ = primero_que_cumple f l in true
  with
   Not_found -> false
;;

primero_que_cumple (function x -> if x>0 then true else false) [0; 0; -2];;

existe (function x -> if x>0 then true else false) [0; 1; -2];;

(*Implementación de la función asociado*)

let asociado l k = 
  snd (primero_que_cumple (function (a,b) -> a == k) l)
;;

let l = [(1,"a"); (2,"b"); (3, "c")] in asociado l 2;;


(*EJERCICIO 3*)

type 'a arbol_binario =
 Vacio
 | Nodo of 'a * 'a arbol_binario * 'a arbol_binario
;;

let rec in_orden t = match t with
  Vacio -> []
  | Nodo (p,lc,rc) -> (in_orden lc) @ (p::in_orden rc)
;;

let rec pre_orden t = match t with
  Vacio -> []
  | Nodo (p,lc,rc) -> (p::pre_orden lc) @ (pre_orden rc)
;;

let rec post_orden t = match t with
  Vacio -> []
  | Nodo (p,lc,rc) -> (post_orden lc) @ (post_orden rc) @ [p]
;;

let anchura arbol = 
  let rec aux cola_nodos recorrido = match cola_nodos with
    [] -> List.rev recorrido
    (*Por que este caso de match?*)
  | Vacio::resto_de_nodos -> aux resto_de_nodos recorrido
  | Nodo(x, izq, der)::resto_de_nodos -> aux (resto_de_nodos @ [izq; der]) (x::recorrido)
  in aux [arbol] []
;;

let arbol =
  Nodo(3,
    Nodo(2,
      Nodo(1, Vacio, Vacio),
      Vacio
    ),
    Nodo(5,
      Nodo(4, Vacio, Vacio),
      Nodo(1, Vacio, Vacio)
    )
  );;

in_orden arbol;;
pre_orden arbol;;
post_orden arbol;;
anchura arbol;;

