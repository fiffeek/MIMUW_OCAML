(* topol.ml - Filip Mikina *)
(* Reviewer : *)


(* some libraries *)
open List;;
open PMap;;

(* exception to raise when approaching
   not a dag *)
exception Cykliczne;;

(* disclaimer : imperative programming was used to
   shorten the code and all the proces of programming in general *)

(*val topol : ('a * 'a list) list -> 'a list
  topol returns a topological order of a given graph *)

let topol l =
  (* ifing the empty list *)
  if l = [] then []
  else
    (* making a graph [not necessary on integers but
       we can make graphs of string etc too *)
    let graf = 
      (* adding a list (h2) to a vertex v1 in a map *)
      ref (List.fold_left (fun acc (h1,h2) -> PMap.add h1 h2 acc) PMap.empty l)
    in let kolory = 
      (* kolory - a map of 'colors' used in dfs algorithm to
         determine which vertex has been done previously than the 
         other

         kolor.(i) = {0,1,2}
         0 - not done
         1 - in the current scc
         2 - already done *)
      ref (List.fold_left (fun acc (h1,h2) -> PMap.add h1 0 acc) PMap.empty l)
    in
    (* stos_wynikowy - a stack to hold the answer
       in ocaml a stack is usually represented by a list *)
    let stos_wynikowy = ref [] in
    (* check v returns kolory.(v) if v is in the kolory map,
       0 otherwise*)
    let check v =
      try PMap.find v !kolory with Not_found -> 0 in
    (* check2 v returns a list of nodes of v if v is in a graph,
       [] otherwise *)
    let check2 v =
      try PMap.find v !graf with Not_found -> [] in
    (* dfs algorithm *)
    let rec dfs v =
      (* it was supposed to be a DAG but if it is not
         we raise the 'Cykliczne' exception *)
      if check v  = 1 then raise Cykliczne
      (* if v hasn't been done yet we do: *)
      else if check v = 0 then
        begin
          (* change the current status od v in map 'kolory' to
             1 (currently doing nodes) *)
          kolory := PMap.add v 1 !kolory;
          (*we extract the info about nodes that are connected to v *)
          let lista_sasiadow = check2 v
          in 
            (* using a (not tail) recursion we apply dfs to
               the nodes in that list *)
            List.iter (fun x -> dfs x) lista_sasiadow;
            (* at this moment v is done and we change that in the map *)
            kolory := PMap.add v 2 !kolory;
            (* we add v to the answer stack *)
            stos_wynikowy := v :: !stos_wynikowy
        end
    in
      (* a graph can have more than one scc so in order to
         have a full list of topological sort we apply dfs to all the 
         nodes on the input *)
      List.iter (fun (x,_) -> dfs x) l;
      (* and finally, we return the stack *)
      !stos_wynikowy;;

(* some testing *)















