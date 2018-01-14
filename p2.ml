(* przelewanka.ml *)
(* Filip Mikina *)
(* Review : *)

(* some libraries *)

open Array;;
open Queue;;

(* gcd [n] [m] returns gcd n and m
   time complexity : O( log (n+m) 
   space com. O(1) *)

let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b);;

(* val przelewanka : (int * int) array -> int *)

let przelewanka tab =
  (* ifing the empty array *)
  if tab = [||] then 0
  else
    let n = Array.length tab in
    let acc_table = Array.make n 0 in
    let max_table = Array.init n (fun x -> fst tab.(x)) in 
    let finish_state = Array.init n (fun x -> snd tab.(x)) in
    (* checking whether there is a zero in finish state array *)
    let is_empty = Array.fold_left (fun acc x -> (acc || x = 0)) false finish_state in
    (* checking whether there is an index in the [tab] array with equal values *)
    let is_full = Array.fold_left (fun acc (x,y) -> (acc || x = y)) false tab in
    (* gcd of all the numbers in finish state array *)
    let gcd_maxes = Array.fold_left (fun acc x -> gcd acc x) max_table.(0) max_table in
    (* gcd of all numbers, but there was no point of not using the [gcd_maxes] one in here *)
    let gcd_all = Array.fold_left (fun acc x -> gcd acc x) gcd_maxes finish_state in
    let states_done = Hashtbl.create 1 in (* not sure about space here // to change *)
    let bfs_queue = Queue.create () in
    (* mem_check [array] [int] adds
       [array] to bfs queue and to hash table
       if it is not in the hash table already *)
    let mem_check copied p_length = 
      if Hashtbl.mem states_done copied = false then
        begin
          Queue.add copied bfs_queue;
          Hashtbl.add states_done copied (p_length + 1)
        end; in
    let answer = ref (-1) in
      (* optimalisation for finish state = [0;0;0...] *)
      if finish_state = acc_table then 0
        (* optimalisation for the case when :
        1) gcd_maxes <> gcd_all then we can not find an answer
        2) number 0 does not exist in the finish state array and there is no number (x,y) in the [tab] array
         that x = y *)
      else if (is_empty = false && is_full = false) || (gcd_all <> gcd_maxes) then !answer
      else
        begin
          Queue.add acc_table bfs_queue;
          Hashtbl.add states_done acc_table 0;
          (* bfs algorithms on states *) (* [every possible state] *)
          while (not (Queue.is_empty bfs_queue)) do
            let queue_state = Queue.pop bfs_queue in
            let path_length = Hashtbl.find states_done queue_state in
              if queue_state = finish_state then 
                begin
                  answer := path_length; (* we found our desired state *)
                  Queue.clear bfs_queue (* we clear queue to stop the loop *)
                end
              else
                begin
                  let table_copied = Array.copy queue_state in
                    for i = 0 to n - 1 do
                      (* adding water to glass i *)
                      table_copied.(i) <- max_table.(i);
                      mem_check (Array.copy table_copied) path_length; (* just to clarify, we copy the array bc of ocaml *)
                      (* pouring water from glass i*)
                      table_copied.(i) <- 0;
                      mem_check (Array.copy table_copied) path_length;
                      (* reseting the value in the table *)
                      table_copied.(i) <- queue_state.(i);
                      for j = 0 to n - 1 do
                        if i <> j then 
                          begin
                            (* pouring water from glass number i to j *)
                            table_copied.(i) <- max 0 (queue_state.(i) - (max_table.(j) - queue_state.(j)));
                            table_copied.(j) <- min max_table.(j) (queue_state.(j) + queue_state.(i));
                            mem_check (Array.copy table_copied) path_length;
                            (* reseting the values in the array *)
                            table_copied.(i) <- queue_state.(i);
                            table_copied.(j) <- queue_state.(j)
                          end
                      done
                    done
                end
          done;
          (* returning the answer *)
          !answer;
        end;;
