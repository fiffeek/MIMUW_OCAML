(* topol.ml - Filip Mikina *)
(* Reviewer : Rafal Maj *)


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
    let graph = ref (List.fold_left (fun acc (h1,h2) -> PMap.add h1 h2 acc) PMap.empty l) in
    (* colors - a map of 'colors' used in dfs algorithm to
       determine which vertex has been done previously than the 
       other
       kolor.(i) = {0,1,2}
       0 - not done
       1 - in the current scc
       2 - already done *)
    let colors = ref (List.fold_left (fun acc (h1,h2) -> PMap.add h1 0 acc) PMap.empty l) in
    (* answer_stack - a stack to hold the answer
       in ocaml a stack is usually represented by a list *)
    let answer_stack = ref [] in
    (* check v returns colors.(v) if v is in the colors map,
       0 otherwise*)
    let check_colors v = try PMap.find v !colors with Not_found -> 0 in
    (* check_graph v returns a list of nodes of v if v is in a graph,
       [] otherwise *)
    let check_graph v = try PMap.find v !graph with Not_found -> [] in
    (* dfs algorithm *)
    let rec dfs v =
      (* it was supposed to be a DAG but if it is not
         we raise the 'Cykliczne' exception *)
      if check_colors v  = 1 then raise Cykliczne
      (* if v hasn't been done yet we do: *)
      else if check_colors v = 0 then
        begin
          (* change the current status od v in map 'colors' to
             1 (currently doing nodes) *)
          colors := PMap.add v 1 !colors;
          (*we extract the info about nodes that are connected to v *)
          let nodes_list = check_graph v in 
            (* using a (not tail) recursion we apply dfs to
               the nodes in that list *)
            List.iter (fun x -> dfs x) nodes_list;
            (* at this moment v is done and we change that in the map *)
            colors := PMap.add v 2 !colors;
            (* we add v to the answer stack *)
            answer_stack := v :: !answer_stack
        end in
      (* a graph can have more than one scc so in order to
         have a full list of topological sort we apply dfs to all the 
         nodes on the input *)
      List.iter (fun (x,_) -> dfs x) l;
      (* and finally, we return the stack *)
      !answer_stack;;
