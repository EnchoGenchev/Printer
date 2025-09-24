(* printer definitions DO NOT MODIFY THESE *)
type printable = B of bool   | U |
                 S of string | L of int list | P of printable * printable

let ex0 = B true
let ex1 = S "Schrute bucks"
let ex2 = U
let ex3 = L [1; 8; 5; 0; 3]
let ex4 = P (P (U, P (P (L [1; -5; 13], U), P (L [0], B true))), S "Hello")
let ex5 = P (P (B false, P (P (L [-21; 53; 12], S "c"), P (L [0], B false))), S "Hello")
let ex6 = P (P (U, P (P (L [15; -15; 213], S "c"), P (P (U, U), S "false"))), S "Hello")
let ex7 = P (S "Stanley nickels", L [1000000])

(* Your code begins here *)

let rec count_u (p: printable) : int = 
  (*implement steps for the count_u function here.
  This function should count the number of U's that exist within the closure p*)

let rec global_or (p : printable) : bool option = 
  (*implement steps for the global_or here.
  This function should compute the logical or of each boolean within the closure p*)

let rec f_on_int_list (f : int-> int) (p : printable) : printable = 
  (*should traverse a printable p and apply given function f to all elements of each list in p.
  HINT: List.map may come in handy!*)

let rec sum_all_ints (p : printable) : int option = 
  (*should return sum of all integers in printable p*)

let rec tostring (p : printable) : string = 
  (*convers printable element p to a string*)







;;
(*Use these lines to test your functions you produce.*)

(*1: *count_u test. Replace ex1 with whichever test you want to run.*)
Printf.printf "%i\n" (count_u ex1);;

(*2: global_or test. Replace ex1 with whichever test you want to run.*)
let test_or = global_or ex1 in 
match test_or with
  | Some v -> Printf.printf "%b\n" v
  | None -> Printf.printf "None \n" ;;

(*3: test for f_on_int_list. Replace ex1 with whichever test you want to run*)
let f x = x + 1 in 
let test_f = f_on_int_list f ex4 in 
Printf.printf "%s\n" (tostring test_f);;

(*4: sum_all_ints test. Replace ex1 with whichever test you want to run*)
let result = sum_all_ints ex1 in 
match result with
  | Some v -> Printf.printf "%i\n" v
  | None -> Printf.printf "None \n";;

(*5: test for toString. Replace ex1 with the test*)
Printf.printf "%s\n" (tostring ex1);;

