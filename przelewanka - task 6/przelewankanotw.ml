(* przelewanka.ml *)
(* Filip Mikina *)
(* Review : *)

open Array;;
open Queue;;

(* val przelewanka : (int * int) array -> int *)

let rec nwd a b =
  if b = 0 then a
  else nwd b (a mod b);;

let przelewanka tab =
  let n = Array.length tab in
  let acc_table = Array.make n 0 in
  let max_table = Array.init n (fun x -> fst tab.(x)) in 
  let finish_state = Array.init n (fun x -> snd tab.(x)) in
  let is_empty = Array.exists (fun x -> x = 0) finish_state in
  let is_full = Array.exists (fun (x,y) -> x = y) tab in
  let states_done = Hashtbl.create 1 in (* add the third if about nwd *)
  let bfs_queue = Queue.create () in
  let mem_check tb p_length = 
    if Hashtbl.mem states_done tb = false then
      begin
        Queue.add tb bfs_queue;
        Hashtbl.add states_done tb (p_length + 1)
      end; in
  let answer = ref (-1) in
    if is_empty = false && is_full = false then answer
    else
      begin
        Queue.add acc_state bfs_queue;
        Hashtbl.add states_done acc_state 0;
        (* bfs algorithms on states *) (* [every possible state] *)
        while (not (Queue.is_empty bfs_queue)) do
          let queue_state = Queue.take bfs_queue in
          let path_length = Hashtbl.find states_done queue_state in
            if queue_state = finish_state then 
              begin
                (* we found our desired state *)
                answer := path_length;
                Queue.clear bfs_queue (* we clear queue to stop the loop *)
              end
            else
              begin
                let table_copied = Array.copy queue_state in
                  for i = 0 to n - 1 do
                    (* adding water to every possible glass *)
                    table_copied.(i) <- max_table.(i);
                    mem_check table_copied path_length;
                    (* pouring water from every possible glass *)
                    table_copied.(i) <- 0;
                    mem_check table_copied path_length;
                    (* reseting the value in the table *)
                    table_copied.(i) <- queue_state.(i);

                    for j = 0 to n - 1 do
                      if i <> j then 
                        begin
                          (* pouring water from glass number i to j *)
                          table_copied.(i) <- max 0 (table_copied.(i) - (max_table.(j) - table_copied.(j)));
                          table_copied.(j) <- min max_table.(j) (table_copied.(j) + table_copied.(i));
                          mem_check table_copied path_length;
                          table_copied.(i) <- queue_state.(i);
                          table_copied.(j) <- queue_state.(j);
                        end
                    done
                  done
              end
        done
      end



