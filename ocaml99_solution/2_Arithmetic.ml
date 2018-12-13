open Base
open Stdio
(*
31. Determine whether a given integer number is prime. (medium)
# not(is_prime 1);;
- : bool = true
# is_prime 7;;
- : bool = true
# not (is_prime 12);;
- : bool = true
*)

let is_prime num =
  let rec aux tmp num =
    if tmp * tmp > num then true
    else if num mod tmp = 0 then false
    else aux (tmp + 1) num
  in
  if num = 1 || num = 0 then false
  else aux 2 (abs num)
;;

(*
32. Determine the greatest common divisor of two positive integer numbers. (medium)
# gcd 13 27;;
- : int = 1
# gcd 20536 7826;;
- : int = 2
*)

let rec gcd m n =
  if n = 0 then m
  else gcd n (m mod n)
;;

(*
33. Determine whether two positive integer numbers are coprime. (easy)
# coprime 13 27;;
- : bool = true
# not (coprime 20536 7826);;
- : bool = true
*)

let coprime m n = (gcd m n) = 1;;

(*
34. Calculate Euler's totient function φ(m). (medium)
# phi 10;;
- : int = 4
# phi 13;;
- : int = 12
*)

let phi num =
  if num = 1 then 1
  else
    let rec aux tmp acc =
      if tmp = num then acc
      else if (coprime tmp num) then aux (tmp + 1) (acc + 1)
      else aux (tmp + 1) acc
    in aux 1 0
;;

(*
35. Determine the prime factors of a given positive integer. (medium)
# factors 315;;
- : int list = [3; 3; 5; 7]
*)

let factors num =
  let rec aux tmp num acc =
    if tmp > num then acc
    else if (num mod tmp) = 0 then aux tmp (num / tmp) (acc @ [tmp])
    else aux (tmp + 1) num acc
  in aux 2 num []
;;

(*
36. Determine the prime factors of a given positive integer (2). (medium)
# factors 315;;
- : (int * int) list = [(3, 2); (5, 1); (7, 1)]
*)

let factors num =
  let rec aux tmp (k, count) num acc =
    if tmp > num then (if count > 0 then acc @ [(k, count)] else acc)
    else if (num mod tmp) = 0 then
      if tmp = k then aux tmp (k, count + 1) (num / tmp) acc
      else aux tmp (tmp, 1) (num / tmp) (if count > 0 then acc @ [(k, count)] else acc)
    else aux (tmp + 1) (k, count) num acc
  in aux 2 (2, 0) num []
;;

(*
37. Calculate Euler's totient function φ(m) (improved). (medium)
# phi_improved 10;;
- : int = 4
# phi_improved 13;;
- : int = 12
*)

let phi_improved num =
  let li = factors num in
  let rec aux acc = function
    | [] -> acc
    | (p, m) :: tl -> aux ((p - 1) * int_of_float (float_of_int p ** (float_of_int m -. 1.)) * acc) tl
  in aux 1 li
;;

(*
38. Compare the two methods of calculating Euler's totient function. (easy)
# timeit phi 10090;;
- : float = 0.002593994140625
# timeit phi_improved 10090;;
- : float = 3.3855438232421875e-05
*)

let timeit f x =
  let t = Unix.gettimeofday() in
    f x;
    Unix.gettimeofday() -. t;
;;

(*
39. A list of prime numbers. (easy)
# List.length (all_primes 2 7920);;
- : int = 1000
*)

let all_primes a b =
  let rec aux tmp acc =
    if tmp = b then acc
    else aux (tmp + 1) (if is_prime tmp then acc @ [tmp] else acc)
  in aux a []
;;

(*
40. Goldbach's conjecture. (medium)
# goldbach 28;;
- : int * int = (5, 23)
*)

let goldbach num =
  let rec aux tmp =
    if is_prime tmp && is_prime (num - tmp) then (tmp, num - tmp)
    else aux (tmp + 1)
  in aux 2
;;

(*
41. A list of Goldbach compositions. (medium)
# goldbach_list 9 20;;
- : (int * (int * int)) list =
[(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
 (20, (3, 17))]
# goldbach_limit 1 2000 50;;
- : (int * (int * int)) list =
[(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
 (1928, (61, 1867))]
*)

let goldbach_list a b =
  let low = if a mod 2 = 1 then a + 1 else a in
  let rec aux tmp acc =
    if tmp > b then acc
    else aux (tmp + 2) (acc @ [(tmp, goldbach tmp)])
  in aux low []
;;

let goldbach_limit a b lmt =
  let low = if a mod 2 = 1 then a + 1 else a in
  let rec aux tmp acc =
    if tmp > b then acc
    else
      match goldbach tmp with
      | (x, y) when (x > lmt && y > lmt) -> aux (tmp + 2) (acc @ [(tmp, goldbach tmp)])
      | _ -> aux (tmp + 2) acc
  in aux low []
;;

(* orignal solution *)
let goldbach_limit a b lim = List.filter (fun (_,(a,b)) -> a > lim && b > lim) (goldbach_list a b);;
