(* Filip Mikina - origami.ml *)
(* Reviewer : Lukasz Piekarski *)

(* modul list, aby uzyc fold_left *)
open List;;

(* Punkt na płaszczyźnie *)
type point = float * float;;


(* Poskładana kartka: ile razy 
   kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int;;


(* [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
   o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
   a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
   od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
   (lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
   w pozostałych przypadkach 0 razy *)
let prostokat ((x1, y1):point) ((x2, y2):point) =
  let pom (x, y) = 
    if(x1 <= x && x <= x2 && y1 <= y && y<= y2) 
    then 1 else 0
  in (pom:kartka);;

(* [kolko p r] zwraca kartkę, reprezentującą 
   kółko domknięte o środku w punkcie [p] i promieniu [r] *)
let kolko ((x1, y1):point) r=
  let pom (x,y) =
    if (x -. x1) *. (x -. x1) +. (y -. y1) *. (y -. y1) > r *. r
    then 0 else 1
  in (pom:kartka);;

(* wyznacznik det. bierze 2 wektory (x1,y1) -> (x2,y2)
   i (x1,y1) -> (x3,y3) i podaje po ktorej stronie prostej przechodzacej
   przez (x1,y1) i (x2,y2) lezy punkt (x3,y3)
   jezeli det < 0 to lezy po prawej
   jezeli det =0 to sa wspolliniowe
   wpp lezy po lewej *)
let det x1 y1 x2 y2 x3 y3 =
  x1 *. y2 +. x2 *. y3 +. x3 *. y1 -. y2 *. x3 -. y3 *. x1 -. y1 *. x2;; 

(* wyznacza odbicie punktu (x,y) wedlug prostej przechodzacej
   przez (x1,y1) i (x2,y2)
   jest to rownanie geometryczne rozpisane na kartce *)
let odbity (x, y) (x1, y1) (x2, y2) =
  match (x1, y1, x2, y2) with
    | (x1, y1, x2, y2) when x1 = x2 && y1 = y2 ->
        failwith "Wrong Input"
    | (x1, y1, x2, y2) when x1 = x2 -> (x +. 2. *. (x1 -. x) , y)
    | (x1, y1, x2, y2) when y1 = y2 -> (x, y +. 2. *. (y1 -. y))
    | _ -> let a = (y2 -. y1) /. (x2 -. x1)
        in let b = -1. *. a *. x2 +. y2 
        in let c = y +. 1. /. a *. x
        in let x_przeciecie = ((c -. b) *. a) /. (a *. a +. 1.)
        in let y_przeciecie = a *. x_przeciecie +. b
        in (x +. 2. *. (x_przeciecie -. x), y +. 2. *. (y_przeciecie -. y));;

(* [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
   punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
   w ten sposób, że z prawej strony prostej 
   (patrząc w kierunku od [p1] do [p2])
   jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
   przebicie po prawej stronie prostej powinno więc zwrócić 0.
   Przebicie dokładnie na prostej powinno zwrócić tyle samo,
   co przebicie kartki przed złożeniem. Po stronie lewej -
   tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
   który nałożył się na punkt przebicia. *)
let zloz ((x1, y1):point) ((x2, y2):point) (k1:kartka) = 
  let det_spec = det x1 y1 x2 y2 
  in let pom (x, y) = 
    let det_spec_xy = det_spec x y
    in
      if det_spec_xy < 0. then 0
      else if det_spec_xy = 0. then k1 (x, y)
      else k1 (x, y) + k1 (odbity (x, y) (x1, y1) (x2, y2))
  in (pom:kartka);;

(* [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] 
   k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
   czyli wynikiem jest złożenie kartki [k] 
   kolejno wzdłuż wszystkich prostych
   z listy *) 
let skladaj (l:((point * point) list)) (k1:kartka) =
  let pom (x:kartka) (((x1, y1):point),((x2, y2):point)) = 
    zloz (x1, y1) (x2, y2) x
  in fold_left pom k1 l;;

(*Some Testing:*)

(*
let op=[((10.0,8.0),(2.0,4.0));((0.0,8.0),(6.0,1.0));((7.0,0.0),(9.0,2.0));((8.0,3.0),(0.0,8.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test0=skladaj op kartka;;
assert (test0 (4.0,0.0)=0);;
let op=[((8.0,6.0),(10.0,7.0));((7.0,2.0),(3.0,2.0));((9.0,0.0),(1.0,9.0));((2.0,5.0),(9.0,10.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test1=skladaj op kartka;;
assert (test1 (9.0,7.0)=0);;
let op=[((8.0,2.0),(2.0,5.0));((5.0,9.0),(5.0,1.0));((8.0,5.0),(7.0,8.0));((8.0,4.0),(4.0,0.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test2=skladaj op kartka;;
assert (test2 (3.0,5.0)=0);;
let op=[((1.0,3.0),(0.0,8.0));((1.0,5.0),(7.0,7.0));((8.0,7.0),(9.0,5.0));((2.0,4.0),(10.0,10.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test3=skladaj op kartka;;
assert (test3 (3.0,8.0)=0);;
let op=[((3.0,7.0),(1.0,1.0));((3.0,10.0),(2.0,3.0));((1.0,8.0),(4.0,7.0));((7.0,3.0),(3.0,8.0))];;
let kartka=kolko (5.,5.) 4. ;;
  let test4=skladaj op kartka;;
assert (test4 (3.0,7.0)=4);;*)
