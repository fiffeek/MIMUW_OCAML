(* przelewanka.ml *)
(* Filip Mikina *)
(* Review : *)

open Array;;
open Queue;;

(* val przelewanka : (int * int) array -> int *)

let rec nwd a b =
  if b = 0 then a
  else nwd b (a mod b);;

let pr tb =
  print_string " printing the tab \n ";
  let n = Array.length tb in
    begin
      for i = 0 to n - 1 do
        print_int tb.(i);
        print_string " "
      done
    end;
    print_string "\n"
;;

let przelewanka tab =
  let n = Array.length tab in
  let acc_table = Array.make n 0 in
  let max_table = Array.init n (fun x -> fst tab.(x)) in 
  let finish_state = Array.init n (fun x -> snd tab.(x)) in
  let is_empty = true in
  let is_full = true in
  let states_done = Hashtbl.create 1 in (* add the third if about nwd *)
  let bfs_queue = Queue.create () in
  let mem_check copied p_length = 
    if Hashtbl.mem states_done copied = false then
      begin
        print_string "adding!:";
        Queue.add copied bfs_queue;
        Hashtbl.add states_done copied (p_length + 1);
        pr copied
      end; in
  let answer = ref (-1) in
    if is_empty = false && is_full = false then !answer
    else
      begin
        Queue.add acc_table bfs_queue;
        Hashtbl.add states_done acc_table 0;
        pr finish_state;
        (* bfs algorithms on states *) (* [every possible state] *)
        (* while (not (Queue.is_empty bfs_queue)) do *)
        for k = 0 to 2 do
          print_string "q size : ";
          print_int (Queue.length bfs_queue);
          print_string "\n";
          let queue_state = Queue.pop bfs_queue in
            pr queue_state;
            let path_length = Hashtbl.find states_done queue_state in
              print_string "q path : ";
              print_int path_length;
              print_string "\n";
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
                      mem_check (Array.copy table_copied) path_length;
                      (* pouring water from every possible glass *)
                      table_copied.(i) <- 0;
                      mem_check (Array.copy table_copied) path_length;
                      (* reseting the value in the table *)
                      table_copied.(i) <- queue_state.(i);

                      for j = 0 to n - 1 do
                        print_string "bf pour";
                        pr table_copied;
                        if i <> j then 
                          begin
                            (* pouring water from glass number i to j *)


                            table_copied.(i) <- max 0 (queue_state.(j) - queue_state.(j)));    
table_copied.(j) <- min max_table.(j) (queue_state.(j) + queue_state.(i));
mem_check (Array.copy table_copied) path_length;
table_copied.(i) <- queue_state.(i);
table_copied.(j) <- queue_state.(j);
end
done
done
end;
print_string "after sending everything";
print_int (Queue.length bfs_queue);
print_string "_________________________________________";
print_string "\n";
done;
!answer;
end;;

let c = [|(3,2);(3,3);(1,0);(12,1)|];;
przelewanka c;;


