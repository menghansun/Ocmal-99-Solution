open Base
open Stdio

type 'a mult_tree = T of 'a * 'a mult_tree list;;

(*
70C. Count the nodes of a multiway tree. (easy)
# count_nodes (T('a', [T('f',[]) ]));;
- : int = 2
*)

let rec count_nodes = function
  | T (_, t) -> List.fold_left ~init:1 ~f:(fun s n -> s + (count_nodes n)) t
;;

(*
70. Tree construction from a node string. (medium)
# let t = T('a', [T('f',[T('g',[])]); T('c',[]);
            T('b',[T('d',[]); T('e',[])])]);;
val t : char mult_tree =
  T ('a',
   [T ('f', [T ('g', [])]); T ('c', []); T ('b', [T ('d', []); T ('e', [])])])
# string_of_tree t;;
- : string = "afg^^c^bd^e^^^"
# tree_of_string "afg^^c^bd^e^^^";;
- : char mult_tree =
T ('a',
 [T ('f', [T ('g', [])]); T ('c', []); T ('b', [T ('d', []); T ('e', [])])])
*)

let rec string_of_tree (T (x, t)) =
  List.fold_left ~init:(String.make 1 x) ~f:(fun s n -> s ^ (string_of_tree n)) t ^ "^"
;;

let tree_of_string s =
  let rec make acc ofs s =
    if ofs >= String.length s || s.[ofs] = '^' then List.rev acc, ofs + 1
    else
      let v = s.[ofs] in
      let l, ofs = make [] (ofs + 1) s in
      make (T (v, l) :: acc) ofs s
  in
  match fst(make [] 0 s) with
  | [x] -> x
  | _ -> failwith "tree_of_string";;
;;

(*
71. Determine the internal path length of a tree. (easy)
# ipl t;;
- : int = 9
*)

let ipl (T (x, t)) =
  let rec aux (T (x, t)) acc =
    List.fold_left ~init:acc ~f:(fun s n -> s + aux n (acc + 1)) t
  in aux (T (x, t)) 0
;;

(*
72. Construct the bottom-up order sequence of the tree nodes. (easy)
# bottom_up (T('a', [T('b', [])]));;
- : char list = ['b'; 'a']
# bottom_up t;;
- : char list = ['g'; 'f'; 'c'; 'd'; 'e'; 'b'; 'a']
*)

let rec bottom_up (T (x, t)) =
  List.fold_right ~init:[x] ~f:(fun n s -> (bottom_up n) @ s) t
;;

(*
73. Lisp-like tree representation. (medium)
# lispy (T('a', []));;
- : string = "a"
# lispy (T('a', [T('b', [])]));;
- : string = "(a b)"
# lispy t;;
- : string = "(a (f g) c (b d e))"
*)

let rec lispy = function
  | T (x, []) -> String.make 1 x
  | T (x, t) -> "(" ^ String.make 1 x ^ List.fold_left ~init:"" ~f:(fun s n -> s ^ " " ^ lispy n) t ^ ")"
;;
