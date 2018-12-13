open Base
open Stdio

type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

(*
55. Construct completely balanced binary trees. (medium)
# cbal_tree 4;;
- : char binary_tree list =
[Node ('x', Node ('x', Empty, Empty),
  Node ('x', Node ('x', Empty, Empty), Empty));
 Node ('x', Node ('x', Empty, Empty),
  Node ('x', Empty, Node ('x', Empty, Empty)));
 Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Empty, Empty));
 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Empty, Empty))]
# List.length(cbal_tree 40);;
- : int = 524288
*)

let rec cbal_tree n =
  let create_tree left right acc =
    let add_left r = List.map ~f:(fun l -> Node ('x', l, r)) left in
    List.concat (List.map ~f:(fun r -> add_left r) right) @ acc
  in
  if n = 0 then [Empty]
  else if n mod 2 = 1 then
    let t = cbal_tree (n / 2) in
    create_tree t t []
  else
    let t1 = cbal_tree (n / 2 - 1) in
    let t2 = cbal_tree (n / 2) in
    create_tree t1 t2 (create_tree t2 t1 [])
;;

(*
56. Symmetric binary trees. (medium)
*)

let rec is_mirror left right =
  match left, right with
  | Empty, Empty -> true
  | Node (_, l, r), Node (_, l', r') -> is_mirror l l' && is_mirror r r'
  | _, _ -> false
;;

let is_symmetric = function
  | Empty -> true
  | Node (_, l, r) -> is_mirror l r
;;

(*
57. Binary search trees (dictionaries). (medium)
# construct [3;2;5;7;1];;
- : int binary_tree =
Node (3, Node (2, Node (1, Empty, Empty), Empty),
 Node (5, Empty, Node (7, Empty, Empty)))
*)

let construct l =
  let rec add_node node n =
    match node with
    | Empty -> Node (n, Empty, Empty)
    | Node (x, l, r) ->
      if n = x then Node (x, l, r)
      else if n < x then Node (x, add_node l n, r)
      else Node (x, l, add_node r n)
  in
  List.fold_left ~f:(add_node) ~init: Empty l;;

(*
58. Generate-and-test paradigm. (medium)
# sym_cbal_trees 5;;
- : char binary_tree list =
[Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Empty, Node ('x', Empty, Empty)));
 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Node ('x', Empty, Empty), Empty))]
*)

let sym_cbal_trees n = List.filter ~f:(is_symmetric) (cbal_tree n);;

(*
59. Construct height-balanced binary trees. (medium)
# let t = hbal_tree 3;;
val t : char binary_tree list =
  [Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Empty, Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Empty, Empty));
   Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
    Node ('x', Empty, Empty));
   Node ('x', Node ('x', Empty, Empty),
    Node ('x', Empty, Node ('x', Empty, Empty)));
   Node ('x', Node ('x', Empty, Empty),
    Node ('x', Node ('x', Empty, Empty), Empty));
   Node ('x', Node ('x', Empty, Empty),
    Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)))]
# let x = 'x';;
val x : char = 'x'
# List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
                 Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)))) t;;
- : bool = true
# List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
                 Node(x, Node(x, Empty, Empty), Empty))) t;;
- : bool = true
# List.length t;;
- : int = 15
*)

let rec hbal_tree n =
  let create_tree left right acc =
    let add_left r = List.map ~f:(fun l -> Node ('x', l, r)) left in
    List.concat (List.map ~f:(fun r -> add_left r) right) @ acc
  in
  if n = 0 then [Empty]
  else if n = 1 then [Node ('x', Empty, Empty)]
  else
    let t1 = hbal_tree (n - 1) in
    let t2 = hbal_tree (n - 2) in
    create_tree t1 t1 (create_tree t1 t2 (create_tree t2 t1 []))
;;

(*
60. Construct height-balanced binary trees with a given number of nodes. (medium)
*)

let max_nodes h = 1 lsl h - 1;;
let rec min_nodes = function
  | 0 -> 0
  | 1 -> 1
  | x -> 1 + min_nodes (x - 1) + min_nodes (x - 2)
;;
let min_height n =
  let rec height h n =
    if max_nodes h >= n then h
    else height (h + 1) n
  in height 0 n
;;
let max_height n =
  let rec height h n =
    if min_nodes h > n then h - 1
    else height (h + 1) n
  in height 0 n
;;

(* todo *)
(*
let hbal_tree_nodes =
*)

(*
61. Count the leaves of a binary tree. (easy)
# count_leaves Empty;;
- : int = 0
# count_leaves example_tree;;
- : int = 3
*)

let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r
;;

(*
61A. Collect the leaves of a binary tree in a list. (easy)
# leaves Empty;;
- : 'a list = []
# leaves example_tree;;
- : char list = ['d'; 'e'; 'g']
*)

let leaves tree =
  let rec leaf acc = function
    | Empty -> []
    | Node (x, Empty, Empty) -> [x]
    | Node (_, l, r) -> leaf acc l @ leaf acc r
  in leaf [] tree
;;

(*
62. Collect the internal nodes of a binary tree in a list. (easy)
# internals (Node('a', Empty, Empty));;
- : char list = []
# internals example_tree;;
- : char list = ['b'; 'a'; 'c'; 'f']
*)

let internals tree =
  let rec leaf acc = function
    | Empty | Node (_, Empty, Empty) -> acc
    | Node (x, l, r) -> leaf (x :: leaf acc r) l
  in leaf [] tree
;;

(*
62B. Collect the nodes at a given level in a list. (easy)
# at_level example_tree 2;;
- : char list = ['b'; 'c']
# at_level example_tree 5;;
- : char list = []
*)

let rec at_level tree n =
  match tree with
  | Empty -> []
  | Node (x, l, r) ->
    if n = 1 then [x]
    else at_level l (n - 1) @ at_level r (n - 1)
;;

(*
63. Construct a complete binary tree. (medium)
# complete_binary_tree [1;2;3;4;5;6];;
- : int binary_tree =
Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
 Node (3, Node (6, Empty, Empty), Empty))
*)

let complete_binary_tree li =
  let rec split acc num = function
    | [] -> (List.rev acc, [])
    | hd :: tl ->
      if num > 0 then split (hd :: acc) (num - 1) tl
      else (List.rev acc, hd :: tl)
  in
  let rec add_node acc li node_list =
    match li, node_list with
    | hd :: tl, l :: r :: t -> add_node (Node (hd, l, r) :: acc) tl t
    | hd :: tl, l :: t -> add_node (Node (hd, l, Empty) :: acc) tl t
    | li, [] -> List.rev acc @ (List.map ~f:(fun x -> Node (x, Empty, Empty)) li)
    | _ -> acc
  in
  match li with
    | [] -> Empty
    | li ->
      let rec aux n = function
        | [] -> []
        | new_li ->
          let l, r = split [] (1 lsl n) new_li in
          add_node [] l (aux (n + 1) r)
      in
      match (aux 0 li) with
      | x :: _ -> x
      | _ -> Empty
;;

(*
64. Layout a binary tree (1). (medium)
# let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                             Node('h', Node('g', leaf 'e',Empty), Empty)),
                   leaf 'm'),
         Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty));;
val example_layout_tree : char binary_tree =
  Node ('n',
   Node ('k',
    Node ('c', Node ('a', Empty, Empty),
     Node ('h', Node ('g', Node ('e', Empty, Empty), Empty), Empty)),
    Node ('m', Empty, Empty)),
   Node ('u', Node ('p', Empty, Node ('s', Node ('q', Empty, Empty), Empty)),
    Empty))

# layout_binary_tree_1 example_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 8, 1),
      Node (('k', 6, 2),
            Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
                  Node (('h', 5, 4),
                        Node (('g', 4, 5), Node (('e', 3, 6), Empty, Empty), Empty), Empty)),
            Node (('m', 7, 3), Empty, Empty)),
      Node (('u', 12, 2),
            Node (('p', 9, 3), Empty,
                  Node (('s', 11, 4), Node (('q', 10, 5), Empty, Empty), Empty)),
            Empty))
*)

let layout_binary_tree_1 tree =
  let rec y_axis n = function
    | Empty -> Empty
    | Node (x, l, r) ->
      Node ((x, 0, n), y_axis (n + 1) l, y_axis (n + 1) r)
  in
  let rec gothrough = function
    | Empty -> []
    | Node (x, l, r) -> gothrough l @ [x] @ gothrough r
  in
  let rec x_list acc n = function
    | [] -> List.rev acc
    | hd :: tl -> x_list ((hd, n) :: acc) (n + 1) tl
  in
  let li = x_list [] 1 (gothrough tree) in
  let rec x_axis = function
    | Empty -> Empty
    | Node ((a, x, y), l, r) -> Node ((a, List.Assoc.find_exn li ~equal:(=) a, y), x_axis l, x_axis r)
  in x_axis (y_axis 1 tree)
;;

(*
65. Layout a binary tree (2). (medium)
# layout_binary_tree_2 example_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 15, 1),
 Node (('k', 7, 2),
  Node (('c', 3, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('e', 5, 4), Node (('d', 4, 5), Empty, Empty),
    Node (('g', 6, 5), Empty, Empty))),
  Node (('m', 11, 3), Empty, Empty)),
 Node (('u', 23, 2),
  Node (('p', 19, 3), Empty, Node (('q', 21, 4), Empty, Empty)), Empty))
# let example2_layout_tree =
    let leaf x = Node (x,Empty,Empty) in
    Node('n', Empty,
         Node('u', Node('p', Empty, leaf 'q'), Empty));;
val example2_layout_tree : char binary_tree =
  Node ('n', Empty,
   Node ('u', Node ('p', Empty, Node ('q', Empty, Empty)), Empty))
# layout_binary_tree_2 example2_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 1, 1), Empty,
 Node (('u', 5, 2),
  Node (('p', 3, 3), Empty, Node (('q', 4, 4), Empty, Empty)), Empty))
*)
(*
let layout_binary_tree_2 t =
*)

let layout_binary_tree_2 tree =
  let rec height n = function
    | Empty -> n
    | Node (x, l, r) -> max (height (n + 1) l) (height (n + 1) r)
  in
  let rec gothrough n p = function
    | Empty -> Empty
    | Node (x, l, r) ->
      Node ((x, p) , gothrough (n - 1) (p - (1 lsl (n - 2))) l, gothrough (n - 1) (p + (1 lsl (n - 2))) r)
  in let new_tree = gothrough (height 0 tree) 0 tree in
  let rec add = function
    | Empty -> 0
    | Node ((x, n), l, r) -> min n (min (add l) (add r))
  in let num = add new_tree in
  let rec x_axis = function
    | Empty -> Empty
    | Node ((x, n), l, r) -> Node ((x, n + 1 - num), x_axis l, x_axis r)
  in
  let rec y_axis n = function
    | Empty -> Empty
    | Node ((m, x), l, r) ->
      Node ((m, x, n), y_axis (n + 1) l, y_axis (n + 1) r)
  in y_axis 1 (x_axis new_tree)
;;

(*
66. Layout a binary tree (3). (hard)
# layout_binary_tree_3 example_layout_tree;;
- : (char * int * int) binary_tree =
Node (('n', 5, 1),
 Node (('k', 3, 2),
  Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('e', 3, 4), Node (('d', 2, 5), Empty, Empty),
    Node (('g', 4, 5), Empty, Empty))),
  Node (('m', 4, 3), Empty, Empty)),
 Node (('u', 7, 2),
  Node (('p', 6, 3), Empty, Node (('q', 7, 4), Empty, Empty)), Empty))
# let example3_layout_tree =
    Node('a', Node('b', Empty, Node('e', Empty, Node('f', Empty, Empty))),
         Node('c', Empty, Node('d', Node('g', Empty, Empty), Empty)));;
val example3_layout_tree : char binary_tree =
  Node ('a', Node ('b', Empty, Node ('e', Empty, Node ('f', Empty, Empty))),
   Node ('c', Empty, Node ('d', Node ('g', Empty, Empty), Empty)))
# layout_binary_tree_3 example3_layout_tree;;
- : (char * int * int) binary_tree =
Node (('a', 3, 1),
 Node (('b', 1, 2), Empty,
  Node (('e', 2, 3), Empty, Node (('f', 3, 4), Empty, Empty))),
 Node (('c', 5, 2), Empty,
  Node (('d', 6, 3), Node (('g', 5, 4), Empty, Empty), Empty)))
*)

(*
let layout_binary_tree_3 =
;;
*)

(*
67. A string representation of binary trees. (medium)
# let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('a', Node('b', leaf 'd', leaf 'e'),
    Node('c', Empty, Node('f', leaf 'g', Empty)));;
val example_layout_tree : char binary_tree =
  Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
   Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))
# string_of_tree example_layout_tree;;
- : string = "a(b(d,e),c(,f(g,)))"
# tree_of_string "a(b(d,e),c(,f(g,)))" = example_layout_tree;;
- : bool = true
# tree_of_string "";;
- : char binary_tree = Empty
*)

let rec string_of_tree = function
  | Empty -> ""
  | Node (x, Empty, Empty) -> String.make 1 x
  | Node (x, l, r) -> String.make 1 x ^ "("^ string_of_tree l ^ "," ^ string_of_tree r ^ ")"
;;

let tree_of_string =
  let rec make ofs s =
    if ofs >= String.length s || s.[ofs] = ',' || s.[ofs] = ')' then
      Empty, ofs
    else
      let v = s.[ofs] in
      if ofs + 1 < String.length s && s.[ofs + 1] = '(' then
        let l, ofs = make (ofs + 2) s in (* skip "v(" *)
        let r, ofs = make (ofs + 1) s in (* skip "," *)
        Node(v, l, r), ofs + 1 (* skip ")" *)
      else Node(v, Empty, Empty), ofs + 1
  in fun s -> fst(make 0 s)
;;

(*
68. Preorder and inorder sequences of binary trees. (medium)
# preorder (Node (1, Node (2, Empty, Empty), Empty));;
- : int list = [1; 2]
# preorder (Node (1, Empty, Node (2, Empty, Empty)));;
- : int list = [1; 2]
# let p = preorder example_tree;;
val p : char list = ['a'; 'b'; 'd'; 'e'; 'c'; 'f'; 'g']
# let i = inorder example_tree;;
val i : char list = ['d'; 'b'; 'e'; 'a'; 'c'; 'g'; 'f']
# pre_in_tree p i = example_tree;;
- : bool = true
*)

let rec preorder = function
  | Empty -> []
  | Node (x, l, r) -> x :: (preorder l) @ (preorder r)
;;

let rec inorder = function
  | Empty -> []
  | Node (x, l, r) -> (inorder l) @ x :: (inorder r)
;;

let pre_in_tree p i =
  let rec add_pos n acc = function
    | [] -> List.rev acc
    | hd :: tl -> add_pos (n + 1) ((hd, n) :: acc) tl
  in
  let p_pos = add_pos 0 [] p in
  let i_pos = add_pos 0 [] i in
  let rec make pstart pend =
    if pstart > pend then Empty
    else
      let v = List.nth_exn p pstart in
      if pstart == pend then Node (v, Empty, Empty)
      else
        let pos = List.Assoc.find_exn i_pos ~equal:(=) v in
        let l_end = List.Assoc.find_exn p_pos ~equal:(=) (List.nth_exn i (pos - 1)) in
        if l_end < pstart then Node (v, Empty, make (pstart + 1) pend)
        else
          Node (v, make (pstart + 1) l_end, make (l_end + 1) pend)
  in make 0 (List.length p - 1)
;;

(*
69. Dotstring representation of binary trees. (medium)
*)

let rec tree_dotstring = function
  | Empty -> "."
  | Node (x, l, r) -> String.make 1 x ^ tree_dotstring l ^ tree_dotstring r
;;

let dotstring_tree =
  let rec make ofs s =
    if ofs >= String.length s || s.[ofs] = '.' then
      Empty, ofs
    else
      let v = s.[ofs] in
      if ofs + 2 < String.length s && s.[ofs + 1] = '.' && s.[ofs + 2] = '.' then
        Node(v, Empty, Empty), ofs + 2
      else
        let l, ofs = make (ofs + 1) s in
        let r, ofs = make (ofs + 1) s in
        Node(v, l, r), ofs
  in fun s -> fst(make 0 s)
;;
