open Base
open Stdio
(*
1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)
# last [ "a" ; "b" ; "c" ; "d" ];;
- : string option = Some "d"
# last [];;
- : 'a option = None
*)

(* standard library solution *)
let last li =
  if List.is_empty li then None
  else List.nth li (List.length li - 1)
;;

(* reimplement solution *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | hd :: tl -> last tl
;;

(*
2. Find the last but one (last and penultimate) elements of a list. (easy)
# last_two [ "a" ; "b" ; "c" ; "d" ];;
- : (string * string) option = Some ("c", "d")
# last_two [ "a" ];;
- : (string * string) option = None
*)

let rec last_two = function
  | [] | [_] -> None
  | [a;b] -> Some (a,b)
  | hd :: tl -> last_two tl
;;

(*
3. Find the k'th element of a list. (easy)
# at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
- : string option = Some "c"
# at 3 [ "a" ];;
- : string option = None
*)

(* standard library solution *)
let at nth li =
  if List.length li < nth then None
  else List.nth li (nth - 1)
;;

(* reimplement solution *)
let rec at nth = function
  | [] -> None
  | hd :: tl ->
    if nth = 0 then None
    else if nth = 1 then Some hd
    else at (nth -1) tl
;;

(*
4. Find the number of elements of a list. (easy)
# length [ "a" ; "b" ; "c"];;
- : int = 3
# length [];;
- : int = 0
*)

let rec length = function
  | [] -> 0
  | hd :: tl -> 1 + length tl
;;

(* tail recursive *)
let rec length_plus_n n = function
  | [] -> n
  | hd :: tl -> length_plus_n (n + 1) tl
let length l = length_plus_n 0 l;;

(*
5. Reverse a list. (easy)
# rev ["a" ; "b" ; "c"];;
- : string list = ["c"; "b"; "a"]
*)

let rev li =
  let rec rev_acc r = function
    | [] -> r
    | hd :: tl -> rev_acc (hd :: r) tl
  in rev_acc [] li
;;

(*
6. Find out whether a list is a palindrome. (easy)
# is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
- : bool = true
# not (is_palindrome [ "a" ; "b" ]);;
- : bool = true
*)

let is_palindrome li =
  if li = List.rev li then true
  else false
;;

(*
7. Flatten a nested list structure. (medium)
# flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
- : string list = ["a"; "b"; "c"; "d"; "e"]
*)

type 'a node =
    | One of 'a
    | Many of 'a node list
;;

let flatten li =
  let rec aux r = function
    | [] -> r
    | One x :: tl -> aux (x :: r) tl
    | Many x :: tl -> aux (aux r x) tl
  in List.rev (aux [] li)
;;

(*
8. Eliminate consecutive duplicates of list elements. (medium)
# compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)

(* page 70 *)
let rec compress = function
  | [] -> []
  | hd :: (hd' :: _ as tl) when hd = hd' -> compress tl
  | hd :: tl -> hd :: compress tl
;;

(*
9. Pack consecutive duplicates of list elements into sublists. (medium)
# pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]
*)

let pack li =
  let rec aux r acc = function
    | [] -> acc
    | hd :: (hd' :: _ as tl) when hd = hd' -> aux (hd :: r) acc tl
    | hd :: tl -> aux [] ((hd :: r) :: acc) tl
  in List.rev (aux [] [] li)
;;

(*
10. Run-length encoding of a list. (easy)
# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*)

let encode li =
  let rec aux num acc = function
    | [] -> acc
    | hd :: (hd' :: _ as tl) when hd = hd' -> aux (num + 1) acc tl
    | hd :: tl -> aux 0 ((num + 1, hd) :: acc) tl
  in List.rev (aux 0 [] li)
;;

(*
11. Modified run-length encoding. (easy)
# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]
*)

type 'a rle =
    | One of 'a
    | Many of int * 'a
;;

let encode li =
  let rec aux num acc = function
    | [] -> acc
    | hd :: (hd' :: _ as tl) when hd = hd' -> aux (num + 1) acc tl
    | hd :: tl ->
      if num > 0 then aux 0 ((Many (num + 1, hd)) :: acc) tl
      else aux 0 ((One hd) :: acc) tl
  in List.rev (aux 0 [] li)
;;

(*
12. Decode a run-length encoded list. (medium)
# decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
*)

let decode li =
  let rec aux r = function
    | [] -> r
    | One x :: tl -> aux (x :: r) tl
    | Many (num, acc) :: tl ->
      if num > 0 then aux (acc::r) (Many (num - 1, acc) :: tl)
      else aux r tl
  in List.rev (aux [] li)
;;

(*
13. Run-length encoding of a list (direct solution). (medium)
# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]
*)

(* same with problem 11*)
let encode li =
  let rec aux num acc = function
    | [] -> acc
    | hd :: (hd' :: _ as tl) when hd = hd' -> aux (num + 1) acc tl
    | hd :: tl ->
      if num > 0 then aux 0 ((Many (num + 1, hd)) :: acc) tl
      else aux 0 ((One hd) :: acc) tl
  in List.rev (aux 0 [] li)
;;

(*
14. Duplicate the elements of a list. (easy)
# duplicate ["a";"b";"c";"c";"d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
*)

let rec duplicate = function
  | [] -> []
  | hd :: tl -> hd :: hd :: duplicate tl
;;

(*
15. Replicate the elements of a list a given number of times. (medium)
# replicate ["a";"b";"c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
*)

let rec replicate li num =
  let rec aux tmp acc = function
    | [] -> acc
    | hd :: tl ->
      if tmp < num then aux (tmp + 1) (hd :: acc) (hd :: tl)
      else aux 0 acc tl
  in List.rev (aux 0 [] li)
;;

(*
16. Drop every N'th element from a list. (medium)
# drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
*)

let drop li num =
  let rec aux tmp acc = function
    | [] -> acc
    | hd :: tl ->
      if tmp < num - 1 then aux (tmp + 1) (hd :: acc) tl
      else aux 0 acc tl
  in List.rev (aux 0 [] li)
;;

(*
17. Split a list into two parts; the length of the first part is given. (easy)
# split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list * string list =
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
# split ["a";"b";"c";"d"] 5;;
- : string list * string list = (["a"; "b"; "c"; "d"], [])
*)

let split li num =
  let rec aux first tmp = function
    | [] -> (List.rev first, [])
    | hd :: tl ->
      if tmp > 0 then aux (hd :: first) (tmp - 1) tl
      else (List.rev first, hd :: tl)
  in aux [] num li
;;

(*
18. Extract a slice from a list. (medium)
# slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
- : string list = ["c"; "d"; "e"; "f"; "g"]
*)

let slice li num1 num2 =
  let rec aux tmp1 tmp2 acc = function
    | [] -> []
    | hd :: tl ->
      if tmp1 > 0 then aux (tmp1 - 1) (tmp2 - 1) acc tl
      else if tmp2 >= 0 then aux tmp1 (tmp2 - 1) (hd :: acc) tl
      else List.rev acc
  in aux num1 num2 [] li
;;

(*
19. Rotate a list N places to the left. (medium)
# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
- : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
- : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
*)

let rotate li num =
  let rec aux tmp last = function
    | [] -> []
    | hd :: tl ->
      if tmp > 0 then aux (tmp - 1) (hd :: last) tl
      else hd :: tl @ List.rev last
  in
  let n = (num mod List.length li + List.length li) mod List.length li in
  aux n [] li
;;

(*
20. Remove the K'th element from a list. (easy)
# remove_at 1 ["a";"b";"c";"d"];;
- : string list = ["a"; "c"; "d"]
*)

let remove_at num li =
  let rec aux tmp acc = function
    | [] -> List.rev acc
    | hd :: tl ->
      if tmp = num then List.rev acc @ tl
      else aux (tmp + 1) (hd :: acc) tl
  in aux 0 [] li
;;

(*
21. Insert an element at a given position into a list. (easy)
# insert_at "alfa" 1 ["a";"b";"c";"d"];;
- : string list = ["a"; "alfa"; "b"; "c"; "d"]
# insert_at "alfa" 3 ["a";"b";"c";"d"];;
- : string list = ["a"; "b"; "c"; "alfa"; "d"]
# insert_at "alfa" 4 ["a";"b";"c";"d"];;
- : string list = ["a"; "b"; "c"; "d"; "alfa"]
*)

let insert_at ele num li =
  let rec aux tmp acc = function
    | [] -> List.rev acc @ [ele]
    | hd :: tl ->
      if tmp = num then List.rev acc @ ele :: hd :: tl
      else aux (tmp + 1) (hd :: acc) tl
  in aux 0 [] li
;;

(*[["c"; "d"; "e"]; ["a"; "b"]]
22. Create a list containing all integers within a given range. (easy)
# range 4 9;;
- : int list = [4; 5; 6; 7; 8; 9]
# range 9 4;;
- : int list = [9; 8; 7; 6; 5; 4]
*)

let range num1 num2 =
  let rec aux tmp1 tmp2 acc =
    if tmp1 <= tmp2 then aux (tmp1 + 1) tmp2 (tmp1 :: acc)
    else acc
  in
  if num1 < num2 then List.rev (aux num1 num2 [])
  else aux num2 num1 []
;;

(*
23. Extract a given number of randomly selected elements from a list. (medium)
# rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
- : string list = ["g"; "d"; "a"]
*)

let rand_select li num =
  let ran l = Random.int (List.length l) in
  let rec new_li pos tmp_li acc = function
    | [] -> acc, List.rev tmp_li
    | hd :: tl ->
      if pos = 0 then hd :: acc, List.rev tmp_li @ tl
      else new_li (pos - 1) (hd :: tmp_li) acc tl
  in
  let rec aux tmp (acc, li) =
    match li with
    | [] -> acc
    | hd :: tl ->
      if tmp = 0 then acc
      else aux (tmp - 1) (new_li (ran li) [] acc li)
  in aux num ([], li)
;;

(*
24. Lotto: Draw N different random numbers from the set 1..M. (easy)
# lotto_select 6 49;;
- : int list = [10; 20; 44; 22; 41; 2]
*)

let lotto_select n m =
  rand_select (range 1 m) n
;;

(*
25. Generate a random permutation of the elements of a list. (easy)
# permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
- : string list = ["a"; "e"; "f"; "b"; "d"; "c"]
*)

let permutation li = rand_select li (List.length li);;

(*
26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium)
# extract 2 ["a";"b";"c";"d"];;
- : string list list =
[["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
*)

(* C(n, k) = C(n-1, k-1) + C(n-1, k) *)
let rec extract k li =
  if k <= 0 then [[]]
  else match li with
    | [] -> []
    | h :: tl -> ((List.map ~f:(fun l -> h :: l) (extract (k-1) tl)) @ (extract k tl))
;;

(*
# group ["a";"b";"c";"d"] [2;1];;
- : string list list list =
[[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
*)

let group li num_li =
  let rec sum = function
    | [] -> 0
    | hd :: tl -> hd + sum tl
  in let diff l1 l2 = List.filter ~f:(fun x -> (not (List.mem ~equal:(=) l2 x))) l1
  in let new_li = List.map ~f:(fun x -> [x]) (extract (sum num_li) li) in
  let split num = function
    | [] -> []
    | hd :: tl ->
      let new_li = extract num hd in
      List.map ~f:(fun x -> [diff hd x] @ [x] @ tl) new_li
  in
  let rec aux1 num acc = function
    | [] -> acc
    | hd :: tl -> aux1 num (acc @ (split num hd)) tl
  in
  let rec aux acc = function
    | [] | [_] -> List.map ~f:(List.rev) acc
    | hd :: tl -> aux (aux1 hd [] acc) tl
  in aux new_li num_li
;;

(* orignal solution *)
let group list sizes =
    let initial = List.map ~f:(fun size -> size, []) sizes in
    let prepend p list =
      let emit l acc = l :: acc in
      let rec aux emit acc = function
        | [] -> emit [] acc
        | (n,l) as h :: t ->
           let acc = if n > 0 then emit ((n-1, p::l) :: t) acc
                     else acc in
           aux (fun l acc -> emit (h :: l) acc) acc t
      in
      aux emit [] list
    in
    let rec aux = function
      | [] -> [ initial ]
      | h :: t -> List.concat (List.map ~f:(prepend h) (aux t))
    in
    let all = aux list in
    let complete = List.filter ~f:(List.for_all ~f:(fun (x,_) -> x = 0)) all in
    List.map ~f:(List.map ~f:snd) complete;;

(*
28. Sorting a list of lists according to length of sublists. (medium)
# length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
- : string list list =
[["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
 ["i"; "j"; "k"; "l"]]
# frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                   ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
- : string list list =
[["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
 ["d"; "e"]; ["m"; "n"]]
*)

let length_sort li =
  let mycompare x y =
    compare (List.length x) (List.length y)
  in
  List.sort mycompare li
;;

let frequency_sort li =
  let rec count num acc = function
    | [] -> acc
    | hd :: (hd' :: _ as tl) when List.length hd = List.length hd' -> count (num + 1) acc tl
    | hd :: tl -> count 0 ((List.length hd, num + 1) :: acc) tl
  in
  let freq = List.rev (count 0 [] (length_sort li)) in
  let fre l = List.Assoc.find_exn freq ~equal:(=) (List.length l) in
  List.sort (fun x y -> compare (fre x) (fre y)) li
;;
