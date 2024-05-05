(*Nos movemos a la carpeta ocaml-talf/src
Ejecutamos ocaml
y escribimos las siguienes lineas*)

#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

let g = gic_of_string "S A B C; a b; S;
                      S -> A B | B C;
                      A -> B A | a;
                      B -> C C | b;
                      C -> A B | a;";;

let g2 = gic_of_string "S A B C D; a b; S;
                      S -> A B D| B C;
                      A -> B A | a;
                      B -> C C | b;
                      C -> A B | a;
                      D -> b;";;

let g3 = gic_of_string "S A B C D; a b; S;
                      S -> A B | B C;
                      A -> B A | a;
                      B -> C C | D b;
                      C -> A B | a;
                      D -> b;";;

let g4 = gic_of_string "S A B C; a b; S;
                        S -> A B | B C;
                        A -> B A | a;
                        B -> C | b;
                        C -> A B | a;";;

let g5 = gic_of_string "S A; a b; S;
                        S -> S A | b;
                        A -> A A | a;";;

(*Ejercicio 1*)

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
                      