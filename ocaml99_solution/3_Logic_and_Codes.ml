open Base
open Stdio

type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

(*
46 & 47. Truth tables for logical expressions (2 variables). (medium)
# table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")));;
- : (bool * bool * bool) list =
[(true, true, true); (true, false, true); (false, true, false);
 (false, false, false)]
*)

let table2 a b expr =
  let rec aux m n = function
    | Var x -> if x = a then m else n
    | Not x -> not (aux m n x)
    | And (x, y) -> (aux m n x) && (aux m n y)
    | Or (x, y) -> (aux m n x) || (aux m n y)
  in List.map ~f:(fun (x, y) -> (x, y, aux x y expr)) [(true, true); (true, false); (false, true); (false, false)]
;;

(*
48. Truth tables for logical expressions. (medium)
# table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")));;
- : ((string * bool) list * bool) list =
[([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
 ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
# let a = Var "a" and b = Var "b" and c = Var "c" in
  table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;
- : ((string * bool) list * bool) list =
[([("a", true); ("b", true); ("c", true)], true);
 ([("a", true); ("b", true); ("c", false)], true);
 ([("a", true); ("b", false); ("c", true)], true);
 ([("a", true); ("b", false); ("c", false)], false);
 ([("a", false); ("b", true); ("c", true)], false);
 ([("a", false); ("b", true); ("c", false)], false);
 ([("a", false); ("b", false); ("c", true)], false);
 ([("a", false); ("b", false); ("c", false)], false)]
*)

let table li expr =
  let rec create_table = function
    | [] -> [[]]
    | hd :: tl -> (List.map ~f:(fun l -> (hd, true) :: l) (create_table tl)) @ (List.map ~f:(fun l -> (hd, false) :: l) (create_table tl))
  in
  let rec eval li = function
    | Var x -> List.Assoc.find_exn li ~equal:(=) x
    | Not x -> not (eval li x)
    | And (x, y) -> (eval li x) && (eval li y)
    | Or (x, y) -> (eval li x) || (eval li y)
  in List.map ~f:(fun x -> (x, eval x expr)) (create_table li)
;;

(*
49. Gray code. (medium)
# gray 1;;
- : string list = ["0"; "1"]
# gray 2;;
- : string list = ["00"; "01"; "11"; "10"]
# gray 3;;
- : string list = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]
*)

let rec gray = function
  | 0 -> []
  | 1 -> ["0"; "1"]
  | x -> (List.map ~f:(fun l -> "0" ^ l) (gray (x - 1))) @ List.rev (List.map ~f:(fun l -> "1" ^ l) (gray (x -1)))
;;

(*
50. Huffman code (hard)
# huffman fs;;
- : (string * string) list =
[("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101");
 ("d", "111")]
# huffman ["a", 10;  "b", 15;  "c", 30;  "d", 16;  "e", 29];;
- : (string * string) list =
[("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")]
*)

type huffman_tree =
  | Empty
  | Node of string * int * huffman_tree * huffman_tree

let huffman fs =
  let mycompare node1 node2 =
    match node1, node2 with
    | (Node (x, y, _, _)), (Node (x', y', _, _)) ->
      if compare y y' = 0 then compare x x'
      else compare y y'
    | _, _ -> -1
  in
  let list_to_huffman li = List.map ~f:(fun (x, y) -> Node (x, y, Empty, Empty)) li in
  let sorted_list li = List.sort mycompare li in
  let rec create_tree = function
    | (Node (x, y, _, _) as left) :: (Node (x', y', _, _) as right) :: tl -> create_tree (sorted_list ((Node (x ^ x', y + y', left, right)) :: tl))
    | [x] -> x
    | x -> Empty
  in
  let rec encode tmp acc = function
    | Empty -> acc
    | Node (name, fre, Empty, Empty) -> (name, tmp) :: acc
    | Node (name, fre, left, right) -> (encode (tmp ^ "0") acc left) @ (encode (tmp ^ "1") acc right)
  in encode "" [] (create_tree (sorted_list (list_to_huffman fs)))
;;
