open List;;

type point = float * float;;
(** Punkt na płaszczyźnie *)

type kartka = point -> int;;

let prostokat ((x1,y1):point) ((x2,y2):point) =
  let pom (x,y) = 
    if(x1 <= x && x <= x2 && y1 <= y && y<= y2) 
    then 1 else 0
  in (pom:kartka);;

let kolko ((x1,y1):point) r=
  let pom (x,y) =
    if (x -. x1) *. (x -. x1) +. (y -. y1) *. (y -. y1) > r *. r
    then 0 else 1
  in (pom:kartka);;

let det x1 y1 x2 y2 x3 y3 =
  x1 *. y2 +. x2 *. y3 +. x3 *. y1 -. y2 *. x3 -. y3 *. x1 -. y1 *. x2;; 

let odbity (x,y) (x1,y1) (x2,y2) =
  match (x1,y1,x2,y2) with
    | (x1,y1,x2,y2) when x1 = x2 && y1 = y2 ->
        failwith "some troubles sir"
    | (x1,y1,x2,y2) when x1 = x2 -> (x +. 2. *. (x1 -. x) , y)
    | (x1,y1,x2,y2) when y1 = y2 -> (x, y +. 2. *. (y1 -. y))
    | _ -> let a = (y2 -. y1) /. (x2 -. x1)
        in let b = -1. *. a *. x2 +. y2 
        in let c = y +. 1. /. a *. x
        in let x_przeciecie = ((c -. b) *. a) /. (a *. a +. 1.)
        in let y_przeciecie = a *. x_przeciecie +. b
        in (x +. 2. *. (x_przeciecie -. x), y +. 2. *. (y_przeciecie -. y));;


let zloz ((x1,y1):point) ((x2,y2):point) (k1:kartka) = 
  let det_spec = det x1 y1 x2 y2 
  in let pom (x,y) = 
    let det_spec_xy = det_spec x y
    in
      if det_spec_xy < 0. then 0
      else if det_spec_xy = 0. then k1 (x,y)
      else k1 (x,y) + k1 (odbity (x,y) (x1,y1) (x2,y2))
  in (pom:kartka);;

let skladaj (l:((point * point) list)) (k1:kartka) =
  let pom (x:kartka) (((x1,y1):point),((x2,y2):point)) = 
    zloz (x1,y1) (x2,y2) x
  in fold_left pom k1 l;;
