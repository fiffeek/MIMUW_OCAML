(* * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, 
   Jacek Chrzaszcz, Filip Mikina
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA*)

(* Interval Set.

   This is an interval set, i.e. a set of integers, where large
   intervals can be stored as single elements. Intervals stored in the
   set are disjoint. *)

(* interval tree (t)
   | empty
   | Node of left_tree * interval * right_tree * height * number
   of elements in subtrees

   [number of elements in Node X equal number od elements in subtrees
   plus number of elements in the inverval
*)

type t =
    | Empty
    | Node of t * (int * int) * t * int * int;;

(* declaration of empty tree / set *)

let empty = Empty;;

(* returns true if the set is empty. 

   Assumptions: None
   Time Complexity: O(1)
   Space Completixy: O(1)*)

let is_empty t =
  if t = Empty then true
  else false;;

(* checking if sum of the elements exceeded max_int 

   Assumptions: None
   Time Complexity: O(1)
   Space Completixy: O(1) *)

let if_bad a =
  if a < 0 then max_int
  else a;;

(* Passes the max_int value 

   Assumptions: None
   Time Complexity: O(1)
   Space Completixy: O(1)
*)

let if_max_int a =
  if a = max_int || a < 0 then true
  else false;;

(* returns the height of a set 

   Assumptions: None
   Time Complexity: O(1)
   Space Completixy: O(1)*)

let height t = 
  match t with
    | Empty -> 0
    | Node(_, _, _, h, _) -> h;;

(* val_set returns the exact number of elements in a subtree/tree 

   Assumptions: None
   Time Complexity: O(1)
   Space Completixy: O(1)
   Post-Assumptions: quantity of any interval is not negative*)

let val_set t =
  match t with
    | Empty -> 0
    | Node(_, (s_low,s_up), _, _, quantity) -> 
        let temp1 = quantity in
        let temp2 = s_up - s_low
        in
          if if_max_int temp1 || if_max_int temp2 || if_max_int (temp2 + 1)
          then max_int
          else if_bad (s_up - s_low + 1 + quantity);;

(* make creates a tree from left_tree (l), interval (k) 
   and right_tree (r) 

   Assumptions: l,k and r are balanced and all intervals are disjoint
   Time Complexity: O(1)
   Space Completixy: O(1)
   Post-Assumptions : a new set is balanced *)

let make l k r =
  let quantity = val_set l + val_set r
  in Node (l, k, r, max (height l) (height r) + 1, if_bad quantity);;

(*[bal l k r] balances a new set from sets l and r and interval k 
  in a way that differences in height arer't more than two.

  Assumptions: l and k are already balanced and all intervals are disjoint
  Time Complexity: O(1)
  Space Completixy: O(1)
  Post-Assumptions: a new set is balanced *)

let bal l k r =
  let hl = height l in
  let hr = height r in
    if hl > hr + 2 then
      match l with
        | Node (ll, lk, lr, _, _) ->
            if height ll >= height lr then make ll lk (make lr k r)
            else
              (match lr with
                | Node (lrl, lrk, lrr, _, _) ->
                    make (make ll lk lrl) lrk (make lrr k r)
                | Empty -> assert false)
        | Empty -> assert false
    else if hr > hl + 2 then
      match r with
        | Node (rl, rk, rr, _, _) ->
            if height rr >= height rl then make (make l k rl) rk rr
            else
              (match rl with
                | Node (rll, rlk, rlr, _, _) ->
                    make (make l k rll) rlk (make rlr rk rr)
                | Empty -> assert false)
        | Empty -> assert false
    else make l k r;;

(* returns the minimal element (interval) of a given set 

   Assumptions: Set is balanced
   Time Complexity: O(log n)
   Space Completixy: O(1) *)

let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(* removes the minimal element (interval) of a given set 

   Assumptions: Set is balanced
   Time Complexity: O(log n)
   Space Completixy: O( log n)
   Post Assumptions: Set without a minimal element is balanced *)

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

(* merges two sets 

   Assuptions: all numbers in t1 are smaller than
   numbers in t2 (difference is at least two)
   Time Complexity: O(1)
   Space Completixy: O(1)
   Post-Assumptions: a new set is balanced *)

let merge t1 t2 =
  match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _ ->
        let k = min_elt t2 in
          bal t1 k (remove_min_elt t2);;

(* cmp compares two intervals 
   assuming that both of them are disjoint
   and returns the interval with smaller numbers 

   Assumptions: a <= b and c <= d, intervals (a,b) and 
   (c,d) are disjoint
   Time Complexity: O(1)
   Space Completixy: O(1) *)

let cmp (a,b) (c,d) =
  if b < c then -1
  else if d < a then 1
  else if (a = c && b = d) then 0
  else failwith "joined_intervals";;

(* [add (x, y) s] returns a set containing the same elements as [s],
   plus all elements of the interval [[x,y]] including [x] and [y].

   Assumptions: [x <= y]
   all intervals are disjoint 
   Time Complexity: O(log n) (n - number of intervals)
   Space Completixy: O(1) (tail recursion) *)

let rec add_one x = function
  | Node (l, k, r, h, q) ->
      let c = cmp x k in
        if c = 0 then Node (l, x, r, h, q)
        else if c < 0 then
          let nl = add_one x l in bal nl k r
        else
          let nr = add_one x r in bal l k nr
  | Empty -> Node (Empty, x, Empty, 1, 0);;

(* makes a tree from two trees and an interval 
   also balances the new one's at the same time 

   (n - number of intervals in a set)

   Assumptions: all intervals are disjoint
   Time Complexity: O(log n)
   Space Completixy: O(log n)
   Post-Assumptions: a new tree is balanced, max differences in height in 
   that tree is two
   Invariant: [p] is a number of elements in l
   and [q] is a number of elements in r
   then height l <= log p and height r <= log q *)

let rec join l v r =
  match (l, r) with
      (Empty, _) -> add_one v r
    | (_, Empty) -> add_one v l
    | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
        if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else
          make l v r;;

(* checks whether b <= a <= c
   if not gives the -1 if a is smaller than both of them and 1
   otherwise 

   Assumptions : b <=c 
   Time Complexity: O(1)
   Space Completixy: O(1)*)

let if_in a (b,c) = 
  if a >= b && a <=c then 0
  else if a < b then -1
  else 1;;

(* [mem x t] returns [true] if [t] contains [x], and [false] otherwise. 

   Assumptions: None
   Time Complexity: O(log n)
   Space Completixy: O(1) (tail recursion) *)

let mem number t =
  let rec pom number t =
    match t with
      | Empty -> false
      | Node (l, k, r, _, _) ->
          if (if_in number k) = 0 then true
          else if (if_in number k) = -1 then pom number l
          else pom number r
  in pom number t;;

(* [iter f t] applies [f] to all continuous intervals in the set [t].
   The intervals are passed to [f] in increasing order.

   Assumptions: None
   Time Complexity: O(n)
   Space Completixy: O(n)
   Invariant: n = p + r where [p] is the number of elements already done *)

let iter f t =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
    loop t;;

(* [fold f t a] computes [(f xN ... (f x2 (f x1 a))...)], where x1
   ... xN are all continuous intervals of t, in increasing order. 

   Assumptions: None
   Time Complexity: O(n)
   Space Completixy: O(n)
   Invariant: acc = n - p where [p] is the number of elements in t *)

let fold f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
        loop (f k (loop acc l)) r in
    loop acc t;;

(* Return the list of all continuous intervals of the given set.
   The returned list is sorted in increasing order.

   Assumptions: None
   Time Complexity: O(n)
   Space Completixy: O(n)
   Invariant: acc = n - p where [p] is the number if elements in t *)

let elements t = 
  let rec loop acc = function
    | Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
    loop [] t;;

(* [below n t] returns the number of elements of [t] that are lesser
   or equal to [n]. If there are more than max_int such elements, 
   the result should be max_int. 

   Assumptions: t is balanced
   Time Complexity: O(log n)
   Space Completixy: O(log n)*)

let below x t =
  let rec below_pom x t =
    match t with
      | Empty -> 0 
      | Node (l, k, r, _, _) ->
          if (if_in x k) = 0 then
            let temp1 = val_set l in
            let temp3 = (x- fst k) in
            let temp2 = (temp3 + 1)
            in
              if (if_max_int temp1 || if_max_int temp2 || if_max_int temp3) 
              then max_int else if_bad (temp1 + temp2)
          else if (if_in x k) = -1 then
            let temp3 = below_pom x l
            in
              if if_max_int temp3 
              then max_int else temp3
          else 
            let temp4 = val_set l
            in let temp5 = below_pom x r
            in let temp6 = snd k - fst k + 1
            in
              if (if_max_int temp4 || if_max_int temp6 || if_max_int temp5
                  || if_max_int (temp4 + temp5) || if_max_int (temp4 + temp6)
                  || if_max_int (temp6 + temp5) || if_max_int (temp6 - 1)) 
              then max_int
              else 
                if_bad (temp4 + temp5 + temp6)
  in below_pom x t;;

(* cmp_2 is similar to if_in but does not return
   0 when c<=a or a<=d 

   Assumptions: None
   Time Complexity: O(1)
   Space Completixy: O(1)*)

let cmp_2 a (c,d) =
  if c < a && a < d then 0
  else if a < d then -1
  else 1;;

(* [split x s] returns a triple [(l, present, r)], where
   [l] is the set of elements of [s] that are strictly lesser than [x];
   [r] is the set of elements of [s] that are strictly greater than [x];
   [present] is [false] if [s] contains no element equal to [x],
   or [true] if [s] contains an element equal to [x]. 

   Assumptions: t is balanced
   Time Complexity: O(log n)
   Space Completixy: O(log n)
   Post-Assumptions: all intervals are disjoint, new set is balanced *)

let split x t=
  let rec loop x = function
    | Empty -> (Empty, false, Empty)
    | Node (l, v, r, _, q) ->
        if fst v = x && snd v = x then (l, true, r)
        else if fst v = x then (l, true, add_one (x + 1, snd v) r)
        else if snd v = x then (add_one (fst v , x - 1) l, true, r)
        else 
          let c = cmp_2 x v in
            if c = 0 then 
              let temp_l_tree = add_one (fst v, x - 1) l
              in let temp_r_tree = add_one (x + 1, snd v) r
              in (temp_l_tree, true, temp_r_tree)
            else if c < 0 then
              let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
            else
              let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in
  let (setl, pres, setr) = loop x t in
    (setl, pres, setr);;


(* [remove (x, y) s] returns a set containing the same elements as [s],
   except for all those which are included between [x] and [y].
   Assumes [x <= y]. 

   Assumptions: t is balanced
   Time Complexity: O(log n) (because split is log n)
   Space Completixy: O(log n) (same here) *)

let remove a t =
  let (fst_a_tree, _, _) = split (fst a) t
  in let (_, _, snd_a_tree) = split (snd a) t
  in merge fst_a_tree snd_a_tree;;

(* returns 'the most left' interval in t, that is:
   let's name that interval P (c,d)

   left_interval (x,y) t returns P
   if d = x
   or d = x - 1
   or c <= x <= d

   otherwise it returns (x,y) [that means every set in t
   is disjoint with (x,y)

   f.e. the tree/set is a one-node [1,1] interval and (x,y) = (3,3)
   then left_interval = (3,3)
   but if the tree is a one-node [1,3] (/[1,2],[2,2],[2,8]) interval 
   and (x,y) = (3,3) then left_interval = [1,3] (/[1,2],[2,2],[2,8])

   Assumptions: None
   Time Complexity: O(log n)
   Space Completixy: O(1) (tail recursion)
   Invariant: x >= every element in left_tree of t and k
*)

let rec left_interval (x, y) t =
  match t with
    | Empty -> (x, y)
    | Node(l, k, r, _, _) ->
        if snd k = x then k
        else if (snd k + 1) = x then k
        else if x <= snd k && fst k <= x then k 
        else if fst k <= x then left_interval (x, y) r
        else 
          let temp1 = left_interval (x,y) l
          in 
            if temp1 <> (x,y) then temp1
            else if (left_interval (x,y) l = (x,y) && y >= fst k) 
            then k else (x,y);;

(*the same as left interval but returns the right one 

  Assumptions: None
  Time Complexity: O(log n)
  Space Completixy: O(1) tail recursion *)

let rec right_interval (x, y) t =
  match t with
    | Empty -> (x, y)
    | Node(l, k, r, _, _) ->
        if fst k = y then k
        else if (fst k - 1) = y then k
        else if y <= snd k && fst k <= y then k 
        else if snd k >= y then right_interval (x, y) l
        else 
          let temp1 = right_interval (x,y) r
          in
            if(temp1 <> (x,y)) then temp1
            else if (temp1 = (x,y) && x <= snd k)
            then k else (x,y);;

(* [add (x, y) s] returns a set containing the same elements as [s],
   plus all elements of the interval [[x,y]] including [x] and [y].
   Assumes [x <= y]. 

   Assumptions: None
   Time Complexity: O(log n) (because of split and join)
   Space Completixy: O(log n) (same) *)

let add a t =
  let l_i_a = left_interval a t 
  in let r_i_a = right_interval a t 
  in
    (* if l_i_a = r_i_a = a then it means that a is disjoint with 
       every interval in t *)
    if l_i_a = r_i_a  && l_i_a = a 
    then add_one a t
    else
      (* l = every interval in t that has the elements smaller than l_i_a *)
      let (l,_,_) = split (fst l_i_a) t
      (* r = every interval in t that has the elements bigger than r_i_a *)
      in let (_,_,r) = split (snd r_i_a) t
      in join l (min (fst l_i_a) (fst a), max (snd r_i_a) (snd a)) r;;

(* some testing *)

(*
let t = empty;;
add (2,2) t;;
let t = empty;;
let t = add (1,2) t;;
let t = add (4,4) t;;
add (2,2) t;;
assert( mem 4 t);;
let t = add (3,3) t;;
assert (mem 3 t);;
cmp_2 3 (1,4);;
let xx = split 3 t;;
let t = empty;;
let t = add (1,1) t;;
let t = add (2,2) t;;
let t = remove (1,2) t;;
let t = add (1,1) t;;
let t = add (3,8) t;;
let t = add (11,12) t;;
let t = add (9,9) t;;
assert ( not (mem 10 t));;
assert (below 10 t = 8);;
let p = add (1,2) empty;;
let p = add (5,5) p;;
let p = add (3,3) p;;
let p = remove (2,2) p;;
assert (mem 3 p);;
assert (not (mem 2 p));;
let s = add (1, max_int) (add (-1, 0) empty);;
assert (below max_int s = max_int);;
let s = add (-min_int, max_int) empty;;
assert (below max_int s = max_int);;
below (-7) s;;
*)

