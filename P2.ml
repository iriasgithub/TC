(*Nos movemos a la carpeta ocaml-talf/src
Ejecutamos ocaml
y escribimos las siguienes lineas*)

#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

let g1 = gic_of_string "S A B C; a b; S;
                      S -> A B | B C;
                      A -> B A | a;
                      B -> C C | b;
                      C -> A B | a;"
;;

let notc1 = cadena_of_string "a b a b";;
let c1 = cadena_of_string "a a a";;

let g2 = gic_of_string "S A B C D; a b; S;
                      S -> A B D| B C;
                      A -> B A | a;
                      B -> C C | b;
                      C -> A B | a;
                      D -> b;"
;;

let g3 = gic_of_string "S A B C D; a b; S;
                      S -> A B | B C;
                      A -> B A | a;
                      B -> C C | D b;
                      C -> A B | a;
                      D -> b;"
;;

let g4 = gic_of_string "S A B C; a b; S;
                        S -> A B | B C;
                        A -> B A | a;
                        B -> C | b;
                        C -> A B | a;"
;;

let g5 = gic_of_string "S A; a b; S;
                        S -> S A | b;
                        A -> A A | a;"
;;

let notc5 = cadena_of_string "a a";;
let c5 = cadena_of_string "b a a";;

(*Ejercicio 1*)

(*Función que dada un gramática dice si está en FNC o no*)
let es_fnc = function
  Gic (_, _, Conjunto p, No_terminal s) -> let rec aux l = match l with
                                            [] -> true
                                            | Regla_gic (No_terminal "S", [Terminal ""])::t -> aux t (*Solo se permite que S derive en Epsilon*)
                                            | Regla_gic (No_terminal _, [No_terminal _; No_terminal _])::t -> aux t (*Producciones de la forma A -> BC*)
                                            | Regla_gic (No_terminal _, [Terminal _])::t -> aux t (*Producciones de la forma A -> a*)
                                            | _ -> false 
                                          in aux p
  | _ -> raise(Failure "No es una gramática válida")
;;

(*Ejercicio 2*)

(*Función que dado un terminal de la cadena a comprobar devuelve un conjunto de los no terminales que lo generan
   basándose en las producciones de una gramática*)
let terminal_generators terminal = function
  Gic(_, _, Conjunto p, _) ->  let rec aux p l = match p with
                                      [] -> l
                                      |Regla_gic(generador,generado)::t -> if generado = [terminal] then aux t (agregar generador l) (*Si la parte derecha de la generación es un simbolo terminal*)
                                                                  else aux t l
                                    in aux p (Conjunto [])
;;
                                    
(*Funcion que devuelve una lista con tuplas, donde cada tupla contiene la posición del elemento (n fila, columna) 
   y un conjunto con los no terminales que derivan ese terminal de la cadena*)                          
let init gic cadena = 
  let rec aux simb_left fila m = match simb_left with 
    [] -> fila
    |h::t -> aux t (((1, m), (terminal_generators h gic))::fila) (m+1) 
  in aux cadena [] 1
;;

(*Función que calcula el producto cartesiano de dos no terminales y lo poda
   de modo que devuelve solo los resultantes que están en el cuerpo de alguna regla*)
let check_cartesian tabla gic coord1 coord2 = 
  let combinaciones = cartesiano (List.assoc coord1 tabla) (List.assoc coord2 tabla)
  in let Gic(_, _, Conjunto reglas, _) = gic
  in let rec aux reglas generadores = match reglas with
    [] -> generadores
  | Regla_gic(generador, [simb1; simb2])::t -> if pertenece (simb1, simb2) combinaciones then aux t (agregar generador generadores)
                                               else aux t generadores
  | _ :: t -> aux t generadores
  in aux reglas conjunto_vacio  
;;

(*k = fila más abajo de x
  x = fila que se quiere rellenar
  y = columna (constante)
  La función realiza k iteraciones (k productos cartesianos podados) para rellenar una celda*)
let no_terminal_generators tabla gic k x y = 
  let rec aux k generadores = match k with 
    0 -> generadores
  | _ -> aux (k-1) (union generadores (check_cartesian tabla gic (x-k, y) (k, y+(x-k))))
  in aux k conjunto_vacio
;;  

  (*Función que dada una coordenada de altura, una coordenada de anchura, una tabla y una gic
     añade a la tabla la fila correspondiente a esa altura y anchura*)
let add_row altura anchura tabla gic = 
  let rec aux tabla j = 
    if j > anchura then tabla
    else aux (((altura, j), (no_terminal_generators tabla gic (altura-1) altura j))::tabla) (j+1)
  in aux tabla 1  (*Inicializamos en la columna 1*)
;;

let get_s = function
  Gic(_,_,_,s) -> s
;;

let check tabla longitud gic = match (List.assoc (longitud, 1)) tabla with
  conjunto -> pertenece (get_s gic) conjunto
| exception Not_found -> false
;;

(*Función que ejecuta cyk*)
let cyk w gic = 
  if (es_fnc gic) then match w with
    [] -> raise (Failure "La cadena no puede estar vacía")
    |l -> let fila1 = init gic w (*Inicializamos la primera fila*)
          in let longitud = List.length l 
          in let rec aux altura anchura tabla = if (altura <= longitud) then aux (altura+1) (anchura-1) (add_row altura anchura tabla gic) (*Mientras no lleguemos a la útima 
                                                llamamos a añadir_fila con las métricas de la altura (para poner id) y la anchura de la fila. Devuelve una tabla con la nueva fila añadida*)
                                                else check tabla longitud gic (*Cuando llegamos a la última fila comprobamos*)

          in aux 2 (longitud-1) fila1 (*En la primera iteración, la fila es la 2 y tiene una longitud de longitud total - 1*)

  else raise(Failure "La gramática debe estar en FNC.")
;;  