(********************)
(* Problem 1: range *)
(********************)

let rec range num1 num2 =
  []

(**********************)
(* Problem 2: flatten *)
(**********************)

let rec flatten l =
  []

(*****************************)
(* Problem 3: remove_stutter *)
(*****************************)

let rec remove_stutter l =
  []

(*******************)
(* Problem 4: sets *)
(*******************)

let rec elem x a =
  match a with
  | [] -> false
  | h::t -> if h = x then true else (elem x t)

let rec subset a b =
  match a with
  | [] -> true
  | h::t -> if (elem h b) then (subset t b) else false

let rec eq a b =
  (subset a b) && (subset b a)

let rec remove x a =
  []

let rec union a b =
  []

let rec diff a b =
  []

(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for range *)
  let _ =
    try
      assert (range 2 5 = [2;3;4;5]);
      assert (range 0 0 = [0])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for flatten *)
  let _ =
    try
      assert (flatten [[1;2];[3;4]] = [1;2;3;4]);
      assert (flatten [[1;2];[];[3;4];[]] = [1;2;3;4])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for remove_stutter *)
  let _ =
    try
      assert (remove_stutter [1;2;2;3;1;1;1;4;4;2;2] = [1; 2; 3; 1; 4; 2]);
      assert (remove_stutter [] = []);
      assert (remove_stutter [1;1;1;1;1] = [1]);
      assert (remove_stutter [1;1;1;1;1;2] = [1;2])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Examples of elem *)
  let _ =
    try
      assert (elem 3 [] = false);
      assert (elem 5 [2;3;5;7;9] = true);
      assert (elem 4 [2;3;5;7;9] = false)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Examples of subset *)
  let _ =
    try
      assert (subset [5] [2;3;5;7;9] = true);
      assert (subset [5;3] [2;3;5;7;9] = true);
      assert (subset [5;4] [2;3;5;7;9] = false)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Examples of eq *)
  let _ =
    try
      assert (eq [5;3;2;9;7] [2;3;5;7;9] = true);
      assert (eq [2;3;7;9] [2;3;5;7;9] = false)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for remove *)
  let _ =
    try
      assert (eq (remove 5 []) []);
      assert (eq (remove 5 [2;3;5;7;9]) [2;3;9;7]);
      assert (eq (remove 4 [2;3;5;7;9]) [2;3;5;9;7]);
      assert (eq (remove 9 [2;3;5;7;9]) [2;5;3;7]);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for union *)
  let _ =
    try
      assert (eq (union [2;3;5] []) [2;3;5]);
      assert (eq (union [5;2] [3;7;9]) [2;3;5;9;7]);
      assert (eq (union [2;3;9] [2;7;9]) [2;3;9;7]);
      assert (eq (union [] [2;7;9]) [2;9;7])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for diff *)
  let _ =
    try
      assert (eq (diff [1;3;2] [2;3]) [1]);
      assert (eq (diff ['a';'b';'c';'d'] ['a';'e';'i';'o';'u']) ['b';'c';'d']);
      assert (eq (diff ["hello";"ocaml"] ["hi";"python"]) ["hello";"ocaml"]);
      assert (eq (diff ["hi";"ocaml"] ["hello";"ocaml"]) ["hi"])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 6 programming questions are incorrect.\n") (!error_count)

let _ = main()
