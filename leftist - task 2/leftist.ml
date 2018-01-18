(* Filip Mikina - drzewa lewicowe *)

(* implementacja struktury drzewa, wezel lub null *)

type 'a queue =
    | Node of 'a queue * 'a * int * 'a queue
    | Null;;

(* empty - puste drzewo *)

let empty = Null;;

(* sprawdzenie czy drzewo a jest puste *)

let is_empty a = 
  if(a = Null) 
  then true 
  else false;;

(* wprowadzenie wyjatku *)

exception Empty;;

(* procedura pomocnicza do joina, podajaca wysokosc danego drzewa
   lub -1 dla Nulla *)

let wys a =
  match a with
    | Null -> -1
    | Node(_, _, h, _) -> h;;

(* join laczacy 2 drzewa *)


let rec join q1 q2=
  match (q1,q2) with
    (* wyrzyucenie pare przypadkow z Nullami *)
    | (Null, q2) -> q2
    | (q1, Null) -> q1
    (* zamienienie drzew miejscami gdy priorytet sie nie zgadza *)
    | (Node (_, v1, _, _), Node (_, v2, _, _)) when v2 < v1-> join q2 q1
    (* wlasciwe polaczenie jednego wezla i jakiegos drzewa
       ten przypadek korzysta z funkcji wys, gdyz tak 
       najlatwiej bylo sie dostac do wysokosci lewego poddrzewa*)
    | (Node(left_v1, v1, _,right_v1), q2) ->
        if ( wys left_v1 <= wys ( join right_v1 q2) ) 
        then Node (( join right_v1 q2) , v1, wys left_v1 + 1, left_v1)
        else Node (left_v1, v1, wys (join right_v1 q2) + 1, (join right_v1 q2));;

(* Add dodaje element do drzewa 
   tworzac jednoelementowe drzewo i
   laczac je z drugim drzewem *)

let add a b =
  join (Node (Null,a,0,Null)) b;;

(* delete_min usuwa korzen i laczy dwa pozostale poddrzewa *)

let delete_min a =
  match a with
    | Null -> raise Empty
    | Node(left, w, h, right) -> (w, join left right);;
