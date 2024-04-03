(*Nos movemos a la carpeta ocaml-talf/src
   Ejecutamos ocaml
   y escribimos las siguienes lineas*)

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

let fst (x, _) = x;;
let mid (_, x, _) = x;;
let snd (_, y) = y;;

let equivalentes a1 a2 = match a1, a2 with 
| Af(estados1, simbolos1, e_inicial1, arcos1, e_final1), Af(estados2, simbolos2, e_inicial2, arcos2, e_final2) -> 
  if (not (igual simbolos1 simbolos2)) then false
  else 
    let rec aux pares_pendientes pares_explorados = 
      match pares_pendientes with 
      | h::t -> if (not (pertenece h pares_explorados)) then 
                  if ((not (pertenece (fst(h)) e_final1)) && (pertenece (snd(h)) e_final2)) || (pertenece (fst(h)) e_final1) && (not (pertenece (snd(h)) e_final2))
                  then false 
                  else aux (pares_pendientes @ (crear_lista_transiciones h simbolos1 arcos1 arcos2)) (agregar h pares_explorados)
                else aux pares_pendientes pares_explorados
      | [] -> true
  in aux [(e_inicial1, e_inicial2)] conjunto_vacio
;;

(*Función que indica el estado al que transicionamos desde un estado inicial con un símbolo*)
let transicionar estado arcos simbolo = 
  let arcos_list = list_of_conjunto arcos in 
  mid (List.find (fun (x, _, y) -> x == estado && y == simbolo) arcos_list) 
;;

let transicionar estado arcos simbolo = 
  let arcos_list = list_of_conjunto arcos in 
  match List.find (fun (Arco_af (x, _, y)) -> x = estado && y = simbolo) arcos_list with
  | Arco_af (x, _, y) -> (x, y)
;;
(*Función que crea una lista de transciones de un par a partir de un conjunto de símbolos *)
let crear_lista_transiciones par simbolos arcos1 arcos2 = 
  let rec aux simbolos_left transiciones = 
    match simbolos_left with 
    | h::t -> aux t ((transicionar (fst(par)) arcos1 h, transicionar (snd(par)) arcos2 h)::transiciones)
    | [] -> transiciones
in aux (list_of_conjunto simbolos) []
;;
