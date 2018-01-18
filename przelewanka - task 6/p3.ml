(* przelewanka.ml *)
(* Filip Mikina *)
(* Review : *)

open Array;;
open Queue;;

(* val przelewanka : (int * int) array -> int *)


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
  let states_done = Hashtbl.create 100 in (* add the third if about nwd *)
  let bfs_queue = Queue.create () in
  let answer = ref (-1) in
    begin
      Queue.add acc_table bfs_queue;
      Hashtbl.add states_done acc_table 0;

      (* bfs algorithms on states *) (* [every possible state] *)
      for k = 0 to 1 do
        print_string "one bfs round \n";


        let queue_state = Queue.pop bfs_queue in
          print_int (Queue.length bfs_queue);
          print_string "\n";
          pr queue_state;
          print_string "\n";
          let path_length = Hashtbl.find states_done queue_state in
            begin
              let table_copied = Array.copy queue_state in
                for i = 0 to n - 1 do
                  (* adding water to every possible glass *)
                  table_copied.(i) <- max_table.(i);
                  if Hashtbl.mem states_done table_copied = false then
                    begin
                      print_string "\n";
                      print_string "adding : \n";
                      Queue.add (Array.copy table_copied) bfs_queue;
                      print_string "peeeking \n";
                      pr (Queue.peek bfs_queue);
                      Hashtbl.add states_done table_copied (path_length + 1);
                    end;
                  (* pouring water from every possible glass *)
                  (* reseting the value in the table *)
                  table_copied.(i) <- queue_state.(i);
                done
            end
      done;
      !answer;
    end;;

let c = [|(3,2);(3,3);(1,0);(12,1)|];;
przelewanka c;;


