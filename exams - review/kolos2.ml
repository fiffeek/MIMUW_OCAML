(* wzrost *)
open List;;

(* najdluzszy spojny fragment scisle rosnacy *)

let rec fold_left f a l = 
  match l with 
    | []   -> a 
    | h::t -> fold_left f (f a h) t;;

let wzrost l =
  let helper =
    fun (l_max,l_ak,el_ak) el ->
      let max_l = length l_max
      in let ak_l = length l_ak
      in if el > el_ak then
          if max_l < (ak_l + 1) then
            (el::l_ak,el::l_ak,el)
          else (l_max, el::l_ak, el)
        else
        if max_l < ak_l then
          (l_ak, [], min_int)
        else 
          (l_max, [], min_int)
  in let (a,_,_) = fold_left helper ([],[],min_int) l
  in rev(a);;

wzrost [];;
wzrost [3;4;0;-1;2;3;7;6;7;8];;
wzrost[-1;3;2;1];;


(* parami rozne elementy *)

open Array;;

let rozne a =
  let dlugosc = length a
  in let i = ref 1
  in let j = ref 0
  in let wyn = ref 0;
  in let odp = ref 0
  in
    while !i < dlugosc && !odp = 0
    do
      if a.(!i-1) <= a.(!i) then odp:= !i;
      i := !i + 1
    done;
    print_int !odp;
    j := !odp - 1;
    while !j >= 0 && !odp < dlugosc do
      if a.(!j) - a.(!odp) = 0 then
        wyn := 1;
      if a.(!j) > a.(!odp) then 
        odp:= !odp + 1
      else 
        j := !j  - 1;
    done; 
    if !wyn = 0 then true else false;;

rozne [|7;2;1;3|];;

let bin l =
  let dlugosc = length l
  in let b = ref 1
  in let e = ref dlugosc
  in
    while !b + 1 < !e
    do
      let sr = (!b + !e) / 2 in
        if l.(sr) < 0 then
          b:=sr
        else e:=sr
    done;
    !b;;

let abs a =
  if a < 0 then a*(-1)
  else a;;

let niemalejacy l =
  let dlugosc = length l
  in let k = bin l
  in
    if abs(l.(k)) <= abs(l.(k+1)) && abs(l.(0)) <= abs(l.(dlugosc-1)) && abs(l.(k)) = abs(l.(0))
    then true
    else false;;

niemalejacy [|-1;1;2|];;

let bin l =
  let dlugosc = length l
  in let b = ref 1
  in let e = ref dlugosc
  in
    while !b + 1 < !e
    do
      let sr = (!b + !e) / 2 in
        if l.(sr) < 0 then
          b:=sr
        else e:=sr
    done;
    !b + 1;;

type 'a elem = {v : 'a; mutable next: 'a lista}
and 'a lista = 'a elem option;;

(* warkocz : 'a lista -> 'a lista -> 'a lista -> unit *)


let warkocz l1 l2 l3 =
  let get (Some x) = x in
  let l = ref l1 in
  let m = ref l2 in
  let r = ref l3 in
    while !l <> None && !m <> None && !r <> None do
      let el = get !l in
      let em = get !m in
      let l' = el.next in
      let m' = em.next in
        el.next <- m';
        em.next <- l';
        l:= (get !r).next;
        r:=l';
        m:=m';
    done;
    if !l = None then
      l:= !r
    else if !m = None then 
      m:= !r;
    while !l <> None && !m <> None do
      let el = get !l in
      let em = get !m in
      let l' = el.next in
      let m' = em.next in
        el.next <- m';
        em.next <- l';
        l:=l';
        m:=m';
    done;;



type 'a tree =Null |
              Node of 'a * 'a tree * 'a tree * 'a tree ref;;

	

let x = Node (3, Node (2, Null,Node (3,Null,Null, ref Null),ref Null) , Node (4,Null,Null,ref Null), ref Null);;



(*Drzewo binarne z fastrygą, to drzewo, w którym każdy węzeł posiada dodatkową
  referencję na następny węzeł w porządku infiksowym. (Ostatni węzeł powinien
  zawierać referencję na Null.) Napisz procedurę fastryguj : α tree → unit
  , która
  sfastryguje dane drzewo. *)

let fastryguj t =
  let nast = ref Null in
  let rec aux t =
    match t with
      | Null -> ()
      | Node (x, l, p, r) -> 
          begin
            aux p;
            r := !nast;
            nast := t;
            aux l;
          end
  in
    aux t;;



(* [PCh] Napisz procedurę lewe_liscie : α tree → unit, która w każdym węźle
   drzewa ustawia referencje tak, aby wskazywały na skrajnie lewy liść 
   (Node, którego
   obaj potomkowie, to Null) będący potomkiem danego węzła. *)

let lewe_liscie t = 
  let rec aux t = 
    match t with
      | Null -> Null
      | Node (x, l, p, r) -> 
          match aux l, aux p with
            | Null, Null -> (r := Null; t )
            | Null, b -> ( r := b; b )
            | a, _ -> ( r := a; a ) 
  in
    aux t; ();;



	

(*ZAD4 (D) *)(*Napisz procedurę potomek 
               : α tree → unit, która w każdym węźle ustawi referencję
               na najgłębiej położony węzeł (Node) będący jego potomkiem.)*)

let potomek t = 
  let rec aux t = 
    match t with
      | Null -> (Null, 0)
      | Node (x, l, p, r) -> 
          let (adres1, dlug1) = aux l in
          let (adres2, dlug2) = aux p in
            if dlug1 = 0 && dlug2 = 0 then (r := t; (t, 1) )
            else
            if dlug1 >= dlug2 then
              (r := adres1; (adres1, dlug1 + 1) )
            else 
              (r := adres2; (adres2, dlug2 + 1) )
  in
  let (_,_) = aux t in ();;

(*insertion sort n^2*)

let wstaw l x =
  (filter (fun y ->y <= x) l) @
    (x :: (filter (fun y ->y >x) l));;
let insertion_sort l = fold_left wstaw [] l;;

(*merge sort*)

let rec merge_sort l =
  let split l =
    fold_left (fun (l1,l2) x ->(l2,x::l1)) ([],[]) l
  and merge l1 l2 =
    let rec mrg a l1 l2 =
      match l1 with
          [] ->(rev a) @ l2 |
          (h1::t1) ->
            match l2 with
                [] ->(rev a) @ l1 |
                135
                  (h2::t2) ->
                  if h1 >h2 then
                    mrg (h2::a) l1 t2
                  else
                    mrg (h1::a) t1 l2
    in
      mrg [] l1 l2
  in
    match l with
        [] ->[]|
        [x] ->[x]|
        _ ->
          let
            (l1,l2) = split l
          in
            merge (merge_sort l1) (merge_sort l2);;

(* quick sort *)

let rec quick_sort l =
  let split l s = (filter (fun y ->y <s) l,
                   filter (fun y ->y = s) l,
                   filter (fun y ->y >s) l)
  in
    if length l <2 then l else
      let s = nth l (Random.int (length l))
      in
      let (ll,le,lg) = split l s
      in
        (quick_sort ll) @ le @ (quick_sort lg);;

(* heap sort *)

module type PRI_QUEUE = sig
  type ’a pri_queue typ kolejek
  val empty_queue : ’a pri_queue pusta kolejka
  val is_empty : ’a pri_queue ->bool czy pusta?
  val put : ’a pri_queue ->’a ->’a pri_queue włożenie elementu
  val getmax : ’a pri_queue ->’a maximum
  val removemax : ’a pri_queue ->’a pri_queue usuń maximum
  exception Empty_Queue gdy kolejka pusta
end;;

let heap_sort l =
  let wloz = fold_left put empty_queue l
  and wyjmij (l,q) = ((getmax q)::l,removemax q)
  in
    iteruj
      ([],wloz)
      wyjmij
      (fun (_,q) ->is_empty q)
      (fun (l,_) ->l);;

(* find and union *)

module type FIND_UNION = sig
  type ’a set
  val make_set : ’a ->’a set
  val find : ’a set ->’a
  val equivalent : ’a set ->’a set ->bool
  val union : ’a set ->’a set ->unit
  val elements : ’a set ->’a list
  val n_of_sets : unit->int
end;;


(* Licznik klas. *)
let sets_counter = ref 0
(* Liczba wszystkich klas. *)
let n_of_sets () = !sets_counter
(* Tworzy nową klasę złożoną tylko z danego elementu. *)
let make_set x =
  let rec v = { elem = x; up = ref v; rank = 0; next = [] }
  in begin
    sets_counter := !sets_counter + 1;
    v
  end
(* Znajduje korzeń drzewa,kompresując ścieżkę. *)
let rec go_up s =
  if s == !(s.up) then s
  else begin
    s.up := go_up !(s.up);
    !(s.up)
  end
(* Znajduje reprezentanta danej klasy. *)
let find s =
  (go_up s).elem
(* Sprawdza,czy dwa elementy są równoważne. *)
let equivalent s1 s2 =
  go_up s1 == go_up s2
(* Scala dwie dane (rozłączne) klasy. *)
let union x y =
  let fx = go_up x
  and fy = go_up y
  in
    if not (fx == fy) then begin
      if fy.rank >fx.rank then begin
        fx.up := fy;
        fy.next <- fx :: fy.next
      end else begin
        fy.up := fx;
        fx.next <- fy :: fx.next;
        if fx.rank = fy.rank then fx.rank <- fy.rank + 1
      end;
      sets_counter := !sets_counter - 1
                                        180
    end
(* Lista elementów klasy. *)
let elements s =
  let acc = ref []
  in
  let rec traverse s1 =
    begin
      acc := s1.elem :: !acc;
      List.iter traverse s1.next
    end
  in begin
    traverse (go_up s);
    !acc
  end
end;;

(* warkocz kuby *)

type 'a elem = {v: 'a; mutable next: 'a lista}
and 'a lista = 'a elem option

let get opt = match opt with
  | None -> assert false
  | Some(v) -> v

let warkocz l1 l2 l3 =
  let next1 = ref (get l1).next and next2 = ref l2 and next3 = ref l3 in
  let last = ref (get l1) in
  let starting = ref true in
    while !next1 <> None || !next2 <> None || !next3 <> None do
      if !starting then starting := false else
        (match !next1 with
          | None -> ()
          | Some(el) -> (!last.next <- Some(el); last := el; next1 := el.next));
      (match !next2 with
        | None -> ()
        | Some(el) -> (!last.next <- Some(el); last := el; next2 := el.next));
      (match !next3 with
        | None -> ()
        | Some(el) -> (!last.next <- Some(el); last := el; next3 := el.next));
    done

let l1 = Some {v = 1; next = Some({v = 4; next = Some({v = 7; next = None})})};;
let l2 = Some {v = 2; next = Some({v = 5; next = None})};;
let l3 = Some {v = 3; next = Some({v = 6; next = Some({v = 8; next = None})})};;

(* przesilenie operatora do sort *)
(* sortowanie po 1 wspolrzednej *)

let sortcustom l =
  sort ( fun (x,y) (a,b) -> compare x a) l;;

sortcustom [(6,2);(3,4)]

(*usual compare*)

let compare a b =
  if a > b then 1
  else if a = b then 0
  else -1;;


let sortcustom l =
  sort ( fun (x,y) (a,b) -> compare x a) l;;

sortcustom [(6,2);(3,4)]

(*not usual compare*)

let compare (a,b) (c,d) =
  if a > c then 1
  else if a < c then -1
  else if b > d then 1
  else if b < d then -1
  else 0;;


let sortcustom l =
  sort ( fun (x,y) (a,b) -> compare (x,y) (a,b)) l;;

sortcustom [(3,2);(3,4);(1,2)]


