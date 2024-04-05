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
let afd11 = af_of_string "0 1 2 3; a b c; 0; 1 3; 0 1 a; 1 1 b; 2 3 a; 2 3 c;";; 
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

(*Ejercicio 2*)

let afd1 = af_of_string "0 1 2 3; a b c; 0; 1 3; 0 1 a; 1 1 b; 2 3 a; 2 3 c;";; 
let afd11 = af_of_string "0 1 2 3; a b c; 0; 1 3; 0 1 a; 1 1 b; 2 3 a; 2 3 c;";; 

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

(*Función que indica el estado o lista de estados al que transicionamos desde un estado inicial con un símbolo, o lista vacía si no hay ninguno*)
let transicionar estado arcos simbolo = 
  let rec aux arcos estados =
    match arcos with
    | Arco_af (origen, destino, literal)::t -> if (origen = estado) && (literal = simbolo) then aux t (destino::estados)
                                              else aux t estados
    | [] -> estados
  in aux (list_of_conjunto arcos) []
;;

(*Función que crea una lista de transciones de un par a partir de un conjunto de símbolos *)
let crear_lista_transiciones par simbolos arcos1 arcos2 = 
  let rec aux simbolos_left transiciones = 
    match simbolos_left with 
    | h::t -> let estado1, estado2 = transicionar (fst(par)) arcos1 h, transicionar (snd(par)) arcos2 h in 
              if estado1 = Estado "NULL" && estado2 = Estado "NULL" then aux t (transiciones) else aux t ((estado1, estado2)::transiciones)
    | [] -> transiciones
in aux (list_of_conjunto simbolos) []
;;

equivalentes afd1 afd11;;

(*Ejercicio 3 a)*)

let escaner_afn list_simbolos af = match af with 
| Af(estados, simbolos, e_inicial, arcos, e_final) -> 
  let rec aux simbolos_pendientes estados_pendientes = 
    match simbolos_pendientes with 
    | h::t -> aux t (transicionar_lista estados_pendientes arcos h) 
    | [] -> if comprobar_finales estados_pendientes e_final then true 
            else false 
  in aux list_simbolos [e_inicial]
;;

let rec comprobar_finales list_estados estados_finales= 
  match list_estados with 
  | h::t -> if pertenece h estados_finales then true 
            else comprobar_finales t estados_finales
  | [] -> false 
;;

let transicionar_lista estados arcos simbolo =
  let rec aux estados_transicionados = function
    | [] -> estados_transicionados
    | estado::resto_estados ->
      let estados_transicionados_estado = transicionar estado arcos simbolo in
      aux (estados_transicionados @ estados_transicionados_estado) resto_estados
  in aux [] estados
;;

let l1_si = [Terminal "a"];;
let l2_si = [Terminal "a"; Terminal "a"; Terminal "c"];;

let l3_no = [Terminal "a"; Terminal "a"];;
let l4_no = [Terminal "b"];;

let afnd1= af_of_string "0 1 2 3; a b c; 0; 1 3; 0 1 a; 0 3 a; 1 2 a; 2 3 c;";; 

scaner_afn l1_si afnd1;;

(*Ejercicio 3 b)*)

let escaner_afnd list_simbolos af = match af with 
| Af(estados, simbolos, e_inicial, arcos, e_final) -> 
  let rec aux simbolos_pendientes estado_pendiente = 
    match simbolos_pendientes with 
    | h::t -> (match transicionar estado_pendiente arcos h with 
              | []  -> false
              | [h] -> aux t h 
              | _   -> false)
    | [] -> pertenece estado_pendiente e_final
  in aux list_simbolos e_inicial
;;

let afd1 = af_of_string "0 1 2 3; a b c; 0; 1 3; 0 2 c; 0 1 a; 1 1 b; 2 3 a; 2 3 c;";;

let l1_si = [Terminal "a"; Terminal "b"; Terminal "b"; Terminal "b"];;
let l2_si = [Terminal "c"; Terminal "a"];;
let l3_si = [Terminal "c"; Terminal "c"];;

let l4_no = [Terminal "c"];;
let l5_no = [Terminal "c"; Terminal "b"];;

escaner_afnd l1_si afd1;;


