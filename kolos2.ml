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








