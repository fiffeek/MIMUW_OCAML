open List;;

(* W schemacie tym mamy następujące elementy zmienne:

   wynik dla pustej listy,
   procedura kumulująca wpływ kolejnych elementów listy na wynik,
   listę do przetworzenia.

   Procedura realizująca powyższy schemat, 
   przeglądająca elementy zgodnie z ich kolejnością na liście, 
   nosi tradycyjną nazwę fold_left i jest zdefiniowana następująco:*)


let rec fold_left f a l = 
  match l with 
      []   -> a |
      h::t -> fold_left f (f a h) t;;


(* Pierwszym parametrem jest procedura kumulująca wynik. 
   Ma ona dwa argumenty: dotychczas obliczony wynik i kolejny element 
   listy do przetworzenia. Drugi argument to wynik dla pustej listy, 
   a trzeci to lista do przetworzenia. *)


let sum l = fold_left (fun x y -> x + y) 0 l;;

sum [1;2;3;1;2;7] (* sum = 16 *);;

let il l = fold_left (fun x y -> x * y) 1 l;;

il [1;2;3;1;2;7];;

let revers l = fold_left (fun l x -> x::l) [] l;;

revers [1;2;3;1;2;7];;


let rec fold_left2 f a l1 l2 = 
  match (l1, l2) with
      ([], [])         -> a |
      (h1::t1, h2::t2) -> fold_left2 f (f a h1 h2) t1 t2 |
      _                -> failwith "Listy różnej długości";;

(* val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a = <fun> *)


let rec fold_right f l a = 
  match l with
      []   -> a |
      h::t -> f h (fold_right f t a);;

(* val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun> *)

(*Kolejny często pojawiający się schemat procedur przetwarzających listy polega na tym, 
  że do wszystkich elementów listy stosujemy to samo przekształcenie. 
  Schemat ten został ujęty w postaci procedury map, która stosuje 
  zadaną procedurę do wszystkich elementów danej listy i zwraca listę wyników. 
  Oto implementacja tej procedury za pomocą fold_right*)

let map f l = fold_right (fun h t -> (f h)::t) l [];;

(*Ostatni schemat procedur przetwarzających listy, jaki przedstawimy w tym wykładzie, 
  to schemat procedury wybierającej z danej listy interesujące nas elementy. 
  Sprawdzamy pewien warunek i zostawiamy tylko elementy spełniające go. 
  Schemat ten realizuje procedura filter:*)

let filter p l = 
  fold_right (fun h t -> if p h then h::t else t) l [];;

let rec shuffle l1 l2 =
  match (l1,l2) with
    | ([],[]) -> []
    | ([],l2) -> l2
    | (l1,[]) -> l1
    | (h1::t1,h2::t2) -> (h1::(h2::(shuffle t1 t2)));;

shuffle [3; 2; 8; 1; 9; 3; 6] [5; 7; 0];;

type tree = 
    | Node of tree * tree
    | Leaf;;

let rec wys_sr t = 
  match t with 
      Leaf -> (-1,0) |
      Node (t1, t2) -> 
        let (h1, d1) = wys_sr t1
        and (h2, d2) = wys_sr t2
        in (max h1 h2 + 1, max (max d1 d2) (h1+h2+2));;

let srednica t = snd (wys_sr t);;

(* Lista większościowa to taka lista, na której 
   ponad połowa elementów jest sobie równa, 
   a ich wartość nazywa się właśnie większością. 
   Napisz procedurę wybierającą większość z listy większościowej; 
   uzasadnij jej poprawność.
*)

let majority l = 
  let rec scan c k l = 
    match l with
        [] -> c |
        h::t -> 
          if k = 0 then scan h 1 t 
          else if c = h then scan c (k+1) t
          else scan c (k-1) t
  in scan (hd l) 0 l;;

(* Napisz procedurę, która dla danej listy funkcji, 
   oblicza funkcję będącą sumą funkcji z danej listy.
*)

let suma l = 
  fold_left 
    (fun a f -> fun x -> a x + f x)
    (fun _ -> 0) l;;

(* Rozszerz rozwiązanie poprzedniego zadania tak, 
   żeby zamiast dodawania można było zastosować 
   dowolną dwuargumentową operację na wynikach funkcji.*)

(*
let zloz op (h::t) x = 
fold_left 
(fun a f -> op a (f x)) (h x) t;;*)

(* Napisz procedurę exists, która dla danego predykatu i listy sprawdzi, 
   czy na liście jest element spełniający predykat. 
   Wykorzystaj wyjątki tak, aby nie przeglądać listy, 
   gdy to już nie jest potrzebne.
*)

exception Exists;;

let exists p l = 
  try fold_left (fun a x -> if p x then raise Exists else a) false l
  with Exists -> true;;

(*Napisz procedurę sumy: int list -> int list, której wynikiem dla danej listy  
  [x_1,\dots, x_n] jest lista: [x_1, x_1 + x_2, x_1+x_2+x_3, 
  \dots, x_1+x_2+\cdots+x_n].*)

let funkcja a x =
  match a with
    | [] -> [x]
    | h::t -> (h + x)::h::t;;

let sumy l =
  fold_left funkcja [] l;;

sumy [1; 5; 2; 7; 12; 10; 5];;

(* Korzystając z filter zaimplementuj alorytm quick-sort *)

let rec qsort l = 
  match l with 
      []   -> [] |
      h::t -> qsort (filter ((>) h) t) @ h :: qsort (filter ((<=) h) t);;

(* Napisz procedurę podzial: int list -> int list list, która dla 
   danej listy liczb całkowitych l = [x_1; x_2; \dots; x_n] 
   podzieli ją na listę list [l_1; \dots; l_k], przy czym:

   l = l_1 @ \dots @ l_k,
   dla każdej listy l_i wszystkie elementy na takiej 
   liście są tego samego znaku,
   k jest najmniejsze możliwe. *)

let funkcja (a,b) x =
  match b with
    | [] -> ((if(x>0) then true else false),[[x]])
    | h::t when a = true -> if(x >= 0) then (true,((x::h)::t))
        else  (false,[x]::h::t)
    | h::t when a = false -> if(x < 0) then (false,((x::h)::t))
        else  (true,[x]::h::t)
    | _::_ -> failwith "rak";;

let podzial l =
  fold_left funkcja (false,[]) l;;

podzial [1;3;0;-2;-2;-4;9];;

(*wyrzuci odwrocona liste ale dziala, a no i boola*)

(*Napisz procedurę prefiksy : int list -> int list list, 
  która dla danej listy liczb całkowitych zwróci listę wszystkich 
  jej prefiksów zakończonych zerem, w kolejności rosnącej ich 
  długości. Przykład: prefiksy [0;1;2;3;0;5;6;0;1] = 
  [[0]; [0;1;2;3;0]; [0;1;2;3;0;5;6;0]]. Zwróć uwagę na złożoność rozwiązania.*)

let funkcja (acc,wyn) x =
  if(x = 0) then (acc,(x::acc)::wyn)
  else (x::acc,wyn);;

let prefiksy l =
  fold_left funkcja ([0],[]) l;;

(snd (prefiksy [0;1;2;3;0;5;6;0;1]));;


(* smieszne zadanie do rozwiniec *)

let rozwin l = 
  fold_right
    (fun h t -> 
       (* Przemnażamy elementy t przez kolejne elementy h, gromadząc iloczyny. *)
       flatten
         (map 
            (fun f -> 
               (* Mnożymy elementy t przez f *)
               (map (fun x -> f::x) t)
            ) h
         )
    ) l [[]];;

let rozwin l = 
  fold_right
    (fun h t -> 
       (* Przemnażamy elementy t przez kolejne elementy h, gromadząc iloczyny. *)
       fold_right 
         (fun f acc ->
            (* Mnożymy elementy t przez f i dorzucamy do acc. *)
            fold_right 
              (fun x a -> (f::x)::a) 
              t acc
         ) h [] 
    ) l [[]];;

rozwin [[1;2;3]; [4]; [5;6]];;

(*[[1; 4; 5]; [1; 4; 6]; [2; 4; 5]; [2; 4; 6]; [3; 4; 5]; [3; 4; 6]].*)

let heads l = flatten (map (function [] -> [] | h::_ -> [h]) l) ;;

let funkcja (pop,acc) x =
  (x,((pop +. x) /. 2.)::acc);;

let usrednienie l =
  match l with
    | [] -> (0.,[])
    | h::[] -> (0.,[])
    | h::t -> fold_left funkcja (h,[]) t;;

rev(snd(usrednienie [1.;2.;3.;4.;5.;6.]));;

let usrednienie l = 
  match l with 
      []   -> [] |
      h::t -> 
        let (_, w) = fold_left (fun (p, v) x -> (x, (x +. p)/. 2. :: v)) (h, []) t
        in rev w;;

usrednienie [1.;2.;3.;4.;5.;6.];;

(* Ćwiczenie [Liczba widocznych elementów]

  Powiemy, że liczba w węźle drzewa jest widoczna, 
  jeżeli na ścieżce od tego węzła do korzenia drzewa
  nie ma większej liczby. W szczególności liczba w 
  korzeniu drzewa jest zawsze widoczna, a liczby mniejsze 
  od niej nie są nigdy widoczne. Napisz procedurę widoczne:
  drzewo \to int, która dla zadanego drzewa (zawierającego 
  wyłącznie liczby nieujemne) obliczy liczbę widocznych liczb. 
  Rozwiązując to zadanie należy skorzystać z procedury fold_tree. 
  Możesz założyć, że w drzewie nie ma liczb ujemnych.

  Rozwiązanie

  Rozwiązanie polega na tym, że fold_tree wyznacza procedurę, 
  która po podaniu największej liczby na ścieżce do korzenia oblicza 
  listę wartości widocznych w poddrzewie. Po wyznaczeniu takiej procedury 
  dla całego drzewa wystarczy jej podać jako maksimum na ścieżce do 
  korzenia wartość mniejszą od wszystkich wartości w drzewie, np. -1.

*)

let widoczne t = 
  let merge x l r = 
    fun k -> if x < k then l k + r k else l x + r x + 1
  in
    fold_tree merge (fun _ -> 0) t (-1);;
