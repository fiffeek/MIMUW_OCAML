type wartosc = { dol : float; 
                 gora : float };;

let wartosc_dokladnosc x p = if (x >= 0.) then { dol = x -. x *. (p /. 100.0) ; 
                                                 gora = x +. x *. (p /. 100.0) }
  else { dol = x +. x *. (p /. 100.0) ; gora = x -. x *. (p /. 100.0)};;

let wartosc_od_do x y = { dol = x ; gora = y};;

let czy x = compare x nan = 0;;

let wartosc_dokladna x = { dol = x ; gora = x }

(* sprawdzenie wartosci w zaleznosci od tego czy przedzial jest odwrotny czy nie
   dodatkowo dodane pare warunkow odnosnie nan i infinity na ktorych program mogl
   dawac zle odpowiedzi *)

let in_wartosc x y = match (x,y) with
  | (a,b) when czy a.dol || czy a.gora -> false
  | (a,b) when a.gora = infinity && a.dol != neg_infinity -> 
      if (b >= a.dol) then true else false
  | (a,b) when a.gora != infinity && a.dol = neg_infinity -> 
      if (b <= a.gora) then true else false
  | (a,b) when a.dol = neg_infinity && a.gora = infinity -> true;
  | (a,b) when a.dol = infinity && a.gora = neg_infinity -> false;
  | (a,b) when a.dol <= a.gora -> if(a.dol <= b && a.gora >= b) 
      then true else false
  | (a,b) when a.dol >= a.gora -> if(a.dol <= b || a.gora >= b) 
      then true else false
  | (_,_) -> false;;

(* minimalna wartosc w zaleznosci od ulozenia przedzialu *)

let min_wartosc x = match (x.dol,x.gora) with
  | (a,b) when czy a || czy b -> nan
  | (a,b) when a <= b -> a
  | (a,b) when (b < a) ->  neg_infinity
  | (_,_) -> nan;;

(*wartosc maksymalna zrobiona analogicznie *)

let max_wartosc x = match (x.dol,x.gora) with
  | (a,b) when czy a || czy b -> nan
  | (a,b) when a <= b -> b
  | (a,b) when b < a -> infinity 
  | (_,_) -> nan;;

let sr_wartosc x = match (min_wartosc x, max_wartosc x) with
  | (a,b) when a = neg_infinity && b = infinity -> nan
  | (a,b) -> (a +. b) /. 2.0;;

let plus a b = match (a.dol,a.gora,b.dol,b.gora) with
  | (c,d,e,f) when czy c || czy d || czy e || czy f -> { dol = nan ; gora = nan }
  | (c,d,e,f) when c <= d && e <= f -> { dol = c +. e; gora = d +. f }
  | (c,d,e,f) when c > d && e > f -> { dol = neg_infinity; gora = infinity}
  | (c,d,e,f) when ((c <= d && e > f) || (c > d && e <= f)) -> 
      if ( (c +. e) > (d +. f) ) then { dol = c +. e; gora = d +. f } 
      else { dol = neg_infinity; gora = infinity }
  | (_,_,_,_) -> { dol = neg_infinity; gora = infinity };; 

let minus a b = let k = (plus a ({dol = -. b.gora; gora = -. b.dol})) in
    match (k.dol,k.gora) with
      | (a,b) when a = -. 0. && b = -. 0. -> { dol = 0. ; gora = 0. }
      | (a,b) when a = -. 0. -> {dol = 0. ; gora = b }
      | (a,b) when b = -. 0. -> {dol = a ; gora = 0.}
      | (a,b) -> {dol = a ; gora = b };;

(* preferuje krotkie nazwy pomocniczych funkcji dlatego krotkie wytlumaczenie :
   t1,t4 - tworzy liste 4-elementowa z podanych zmiennych (tj. laczy skrajne wartosci
   w przedzialach,

   t2 - podaje maxa tej listy,

   t3 - podaje min tej listy (tak wiem ze dalo sie napisac w 1 funckji 
   ale copy paste byl szybszy i nie bruzdzi az tak w kodzie,

   razyd a b - bierze 2 'dobre' (nieodwrotne) przedzialy i korzysta z ww. funkcji
   aby ustalic iloczyn przedzialow,

   test f1 f2 - bierze dwa wyniki iloczynu dwoch przedzialow 'dobrych'
   i podaje wynik sklejenia tych przedzialow,

   Ad. test f1 f2 -> rozdzielam odwrotny przedzial na dwa przedzialy dobre
   i potem wymnazam kazdy z nich osobno z drugim 'dobrym' przedzialem z wejscia
   a test skleja te wyniki (podane jako argumenty),

   razy2 a b - ustala jak ma pomnozyc 2 przedzialy i je mnozy (przypadki
   typu jeden odwrotny, drugi nie albo dwa odwrotne etc.)

   razy a b - dostaje wynik z razy2 a b i przetwarza go aby nie bylo bugow
   (problemy z -0. albo nan lub inf etc)*)


let t1 a b c d = [if( czy (a *. c)) then neg_infinity else a *. c;
                  if( czy (a *. d)) then neg_infinity else a *. d;
                  if( czy (c *. b)) then neg_infinity else c *. b; 
                  if( czy (d *. b)) then neg_infinity else d *. b];;

let t4 a b c d = [if( czy (a *. c)) then infinity else a *. c;
                  if( czy (a *. d)) then infinity else a *. d;
                  if( czy (c *. b)) then infinity else c *. b; 
                  if( czy (d *. b)) then infinity else d *. b];;

let rec t2 t1 m1 = match t1 with
  | [] -> m1
  | h::l -> if ( h > m1) then t2 l h
      else t2 l m1;;

let rec t3 t1 m1 = match t1 with
  | [] -> m1
  | h::l -> if ( h < m1) then t3 l h
      else t3 l m1;;

let razyd a b = match (a.dol,a.gora,b.dol,b.gora) with
  | (c,d,e,f) -> { dol = (t3 (t4 c d e f) 
                            (if (czy (e *.c)) then infinity else e *.c )); 
                   gora = (t2 (t1 c d e f) 
                             (if (czy (e *.c)) then neg_infinity else e *.c)) };;

let test f1 f2 = match (f1,f2) with
  | (a,b) when (a.gora < b.dol) -> { dol = b. dol ; gora = a.gora}
  | (a,b) when (a.gora = b.dol) -> { dol = a.dol ; gora = b.gora}
  | (a,b) when (a.gora >= b.dol && a.dol <= b.dol && a.gora <= b.gora) -> 
      {dol = a.dol ; gora = b.gora}
  | (a,b) when (a.gora >= b.dol && a.dol <= b.dol && a.gora >= b.gora) -> 
      {dol = a.dol ; gora = a.gora}
  | (a,b) when (a.dol >= b.dol && a.dol <= b.gora && a.gora >=b.gora) -> 
      {dol = b.dol ; gora = a.gora}
  | (a,b) when (a.dol >= b.dol && a.dol <= b.gora && a.gora <=b.gora) -> 
      {dol = b.dol ; gora = b.gora}
  | (a,b) when (a.dol > b.gora) -> {dol = a.dol ; gora = b.gora}
  | (a,b) when (a.dol = b.gora) -> {dol = b.dol ; gora = a.gora}
  | (_,_) -> { dol = f2.dol ; gora = f1.gora};;


let razy2 a b = match (a.dol,a.gora,b.dol,b.gora) with
  | (c,d,e,f) when czy c || czy d || czy e || czy f -> { dol = nan ; gora = nan }
  | (c,d,e,f) when (e = 0. && f = 0.) || (c = 0. && d = 0.) -> {dol = 0. ; gora = 0.}
  | (c,d,e,f) when c = neg_infinity && d = infinity && e = 0. && f = 0. -> 
      {dol = 0. ; gora = 0.}
  | (c,d,e,f) when c = neg_infinity && d = infinity && (e <> 0. || f <> 0.) -> 
      {dol = neg_infinity ; gora = infinity}
  | (c,d,e,f) when e = neg_infinity && f = infinity && (c <> 0. || d <> 0.) -> 
      {dol = neg_infinity ; gora = infinity}
  | (c,d,e,f) when (c <= d && e <= f) -> razyd a b
  | (c,d,e,f) when (c <= d && e > f) ->  test (razyd a {dol = neg_infinity ; gora = f})
                                           (razyd a {dol = e ; gora = infinity})
  | (c,d,e,f) when (c > d && e <= f) -> test (razyd b {dol = neg_infinity; gora = d}) 
                                          (razyd b {dol = c; gora = infinity})
  | (c,d,e,f) when (c > d && e > f) ->  
      let k = test (razyd {dol = neg_infinity ; gora = f} 
                      {dol = neg_infinity; gora = d}) 
                (razyd {dol = e ; gora = infinity} {dol = c; gora = infinity})
      and t = test (razyd {dol = neg_infinity ; gora = f} {dol = c; gora = infinity}) 
                (razyd {dol = neg_infinity ; gora = d} {dol = e; gora = infinity})
      in if( k.dol > t.gora ) then {dol = k.dol ; gora = t.gora }
        else {dol = neg_infinity ; gora = infinity }
  | (_,_,_,_) -> {dol = infinity ; gora = infinity};;


let razy a b = let k = (razy2 a b).dol and l = (razy2 a b).gora
  in match (k,l) with
    | (c,d) when czy c || czy d -> {dol = nan ; gora = nan }
    | (c,d) when c >= d && d = neg_infinity && c = -0. -> {dol = 0. ; gora = infinity}
    | (c,d) when c >= d && d = neg_infinity -> {dol = c ; gora = infinity}
    | (c,d) when c >= d && c = infinity && d = -0.-> {dol = neg_infinity ; gora = 0.}
    | (c,d) when c >= d && c = infinity -> {dol = neg_infinity ; gora = d}
    | (c,d) when c = -0. && d = -0. -> {dol = 0. ; gora = 0.}
    | (c,d) when c = -0. -> {dol = 0. ; gora = d }
    | (c,d) when d = -0. -> {dol = c ; gora = 0. }
    | (c,d) -> { dol = k ; gora = l};;

(* w podzielic odwracam jeden przedzial i mnoze go z drugim, plus
   pare przypadkow ktore wywalaly testy zostaly wyifowane *)

let podzielic a b = match (a,b) with
  | (c,d) when czy c.dol || czy c.gora || czy d.dol || czy d.gora -> {dol = nan; gora = nan}
  | (c,d) when d.dol = 0. && d.gora = 0. ->  {dol = nan ; gora = nan}
  | (c,d) when (a.gora = 0. && a.dol = 0.) -> {dol = 0. ; gora = 0. }
  | (c,d) when d.dol = neg_infinity && d.gora = infinity -> razy a b
  | (c,d) -> razy a {dol = 1. /. b.gora ; gora = 1. /. b.dol };;
