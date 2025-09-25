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
  match p with 
  | U -> 1 
  | P(p1, p2) -> count_u p1 + count_u p2 
  | _ -> 0



  let rec global_or (p : printable) : bool option = 
  (*implement steps for the global_or here.
  This function should compute the logical or of each boolean within the closure p*)
    match p with 
  | B b -> Some b
  | P (p1, p2) -> (*parenthesis around match so compiler stop freaking out*)
      (match (global_or p1, global_or p2) with
       | Some b1, None -> Some b1
       | None, Some b2 -> Some b2
       | Some b1, Some b2 -> Some (b1 || b2)
       | _ -> None) 
  | _ -> None


let rec f_on_int_list (f : int-> int) (p : printable) : printable = 
  (*should traverse a printable p and apply given function f to all elements of each list in p.
  HINT: List.map may come in handy!*)
  match p with
  | L l -> L (List.map f l) (*added L because has to return type printable*)
  | P (p1, p2) -> P (f_on_int_list f p1, f_on_int_list f p2)
  | _ -> p (*leaving it the same*)

(*helper function to *)
let rec sum_list (l : int list) : int = 
  match l with
  | [] -> 0
  | x::xs -> x + sum_list xs

let rec sum_all_ints (p : printable) : int option = 
  (*should return sum of all integers in printable p*)
  match p with
  | L l -> Some (sum_list l) (*parenthesis so it doesn't freak out*)
  | P (p1, p2) -> (
      match (sum_all_ints p1, sum_all_ints p2) with
      | (Some s1, Some s2) -> Some (s1 + s2)
      | (Some s, None) | (None, Some s) -> Some s
      | (None, None) -> None
    )
  | _ -> None


(*helper function for tostring*)
let rec string_of_int_list (l : int list) : string =
  match l with
  | [] -> ""
  | x::xs -> string_of_int x ^ string_of_int_list xs (*adding element and calling recursively*)

let rec tostring (p : printable) : string = 
  (*convers printable element p to a string*)
  match p with
  | P (p1, p2) -> tostring p1 ^ tostring p2
  | B b -> string_of_bool b
  | U -> "U"
  | S s -> s
  | L l -> string_of_int_list l
  (*exhaustive because covers all types*)

;;
(*Use these lines to test your functions you produce.*)

(*1: *count_u test. Replace ex1 with whichever test you want to run.*)
(*
Printf.printf "%i\n" (count_u ex0);;
Printf.printf "%i\n" (count_u ex1);;
Printf.printf "%i\n" (count_u ex2);;
Printf.printf "%i\n" (count_u ex3);;
Printf.printf "%i\n" (count_u ex4);;
Printf.printf "%i\n" (count_u ex5);;
Printf.printf "%i\n" (count_u ex6);;
Printf.printf "%i\n" (count_u ex7);;
*)



(*2: global_or test. Replace ex1 with whichever test you want to run.*)
(*
let test_or = global_or ex5 in 
match test_or with
  | Some v -> Printf.printf " Some %b\n" v
  | None -> Printf.printf "None \n" ;;




(*3: test for f_on_int_list. Replace ex1 with whichever test you want to run*)
let f x = x + 1 in 
let test_f = f_on_int_list f ex4 in 
Printf.printf "%s\n" (tostring test_f);;
*)

(*4: sum_all_ints test. Replace ex1 with whichever test you want to run*)
let result = sum_all_ints (f_on_int_list (fun t-> t*t) ex6) in 
match result with
  | Some v -> Printf.printf "%i\n" v
  | None -> Printf.printf "None \n";;


(*
(*5: test for toString. Replace ex1 with the test*)
Printf.printf "%s\n" (tostring ex1);;
*)


