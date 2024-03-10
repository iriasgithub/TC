#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

let afne1 = af_of_string "0 1 2 3; a b c; 0; 1 3; 0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c;";;
let afnd1= af_of_string "0 1 2 3; a b c; 0; 1 3; 0 1 a; 0 3 a; 1 2 a; 2 3 c;";; 
let afd1 = af_of_string "0 1 2 3; a b c; 0; 1 3; 0 1 a; 1 1 b; 2 3 a; 2 3 c;";; 
let afde1 = af_of_string "0 1 2 3; a b c; 0; 1 3; 0 1 epsilon; 1 3 epsilon; 1 2 a; 2 3 c;";; 


(*Implemente una función es_afne : Auto.af -> bool que reciba como argumento un autómata
finito, y que devuelva true si se trata de un autómata que presenta alguna épsilon-transición, o false en
caso contrario.*)

let es_epsilon arco = match arco with 
  Arco_af(_, _, Terminal "") -> true 
  | _ -> false
;;

let es_afne af = match af with 
  Af (_, _, _, c, _) -> List.exists es_epsilon (list_of_conjunto c) 
;;



(*Implemente una función es_afn : Auto.af -> bool que reciba como argumento un autómata
finito, y que devuelva true si se trata de un autómata que presenta algún tipo de no determinismo
(excepto épsilon-transiciones), o false en caso contrario.*)

(*Un af es un afn cuando un estado y un símbolo de entrada no necesariamente determinan una única transición*)
(*Dado un conjunto quiero saber si existen elementos iguales solo en dos atributos de cada elemento*)

(*La función simplificar convierte una lista de tripletes en una lista de tuplas en las que solo se tome el estado 1 y el símbolo de entrada*)
let simplificar l = List.map (function Arco_af(e1, e2, t) -> (e1, t)) l ;;

(*Devuelve true si existe alguna tupla repetida, false en caso contrario*)
(*let rec no_determinista l = match l with 
  | [] -> false
  | (_, Terminal "")::t -> no_determinista t (* Ignorar si 'a' es igual a Terminal "" *)
  | h::t -> if (List.mem h t) then true else no_determinista t
;;*)

let rec no_determinista l = match l with 
  | [] -> false
  | h::t -> if (List.mem h t) then true else no_determinista t
;;

let es_afn af = match af with 
  Af(_, _, _, c, _) -> no_determinista (simplificar (list_of_conjunto c))
;;



(*Implemente una función es_afd : Auto.af -> bool que reciba como argumento un autómata
finito, y que devuelva true si se trata de un autómata totalmente determinista, o false en caso contrario.*)

let rec no_determinista_ni_epsilon l = match l with 
| [] -> false
| (_, Terminal "")::t -> true (*Si se encuentra alguna epsilon transicion ya no es totalmente determinista*)
| h::t -> if (List.mem h t) then true else no_determinista_ni_epsilon t (*Si se encuentrane elementos repetidos ídem*)
;;

let es_afd af = match af with 
  Af(_, _, _, c, _) -> not (no_determinista_ni_epsilon (simplificar (list_of_conjunto c)))
;;