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

(*EJERCICIO 4*)

type 'a conjunto = Conjunto of 'a list;;

let conjunto_vacio = Conjunto [];;

let rec pertenece a c = match c with
  Conjunto ([]) -> false
  | Conjunto (h::t) -> if h == a then true else pertenece a (Conjunto(t))
;;

let pertenece a = function (*recordar que el segundo parámetro es Conjunto l*)
    Conjunto l -> List.mem a l
;;

let c = Conjunto([1;2;3;4;5]);;
pertenece 3 c;;

let agregar a = function Conjunto l -> if pertenece a (Conjunto l) then Conjunto l
                                      else Conjunto (a::l)
;;

agregar 19 c;;
agregar 1 c;;

let conjunto_of_list l = Conjunto l;;

let suprimir a = function Conjunto l -> Conjunto (List.filter (fun x -> x <> a) l);;

suprimir 1 c;;

let cardinal = function Conjunto l -> List.length l;;

cardinal c;;

let union = function Conjunto l1 -> function Conjunto l2 -> 
  conjunto_of_list (List.sort_uniq compare (l1 @ l2))
;;

let interseccion = function Conjunto l1 -> function Conjunto l2 ->
  conjunto_of_list  (List.filter (fun x -> List.mem x l2) l1)
;;

let c2 = Conjunto([1;2;6;7;8]);;
union c c2;;
interseccion c c2;;

let diferencia = function Conjunto l1 -> function Conjunto l2 ->
  conjunto_of_list  (List.filter (fun x -> not (List.mem x l2)) l1)
;;

diferencia (union c c2) (interseccion c c2);;

(*Mirar si el conjunto c1 está incluido en el conjunto c2*)
(*for_all mira si todos los elementos de la lista satisfacen el predicado, [] -> true*)
let incluido c1 c2 = match c1 with Conjunto l1 ->
 List.for_all (fun x -> pertenece x c2) l1
;;

let c3 = Conjunto([1;2]);;
incluido c c2;;
incluido c3 c;;

let igual c1 c2 = (incluido c1 c2) && (incluido c2 c1);;

let c4 = Conjunto([1;2]);;
igual c c2;;
igual c3 c4;;

let rec lprod l1 l2 = match l1 with
  [] -> []
  |h::t -> List.map (fun x -> (h,x)) l2 @ lprod t l2
;;

let producto_cartesiano = function Conjunto l1 -> function Conjunto l2 ->
  conjunto_of_list (lprod l1 l2)
;;

producto_cartesiano c c2;;
producto_cartesiano c3 c4;;

let list_of_conjunto = function Conjunto l -> l;;

list_of_conjunto c2;;