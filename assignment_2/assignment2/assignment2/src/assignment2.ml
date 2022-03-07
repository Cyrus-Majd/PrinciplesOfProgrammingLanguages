open List

let rec cond_dup l f =
  match l with
  | [] -> []
  | hd::tl -> if f hd = true then hd::hd::cond_dup tl f else hd::cond_dup tl f;;

let rec n_times (f, n, v) =
  if n <= 0 then v else n_times(f, n-1, f v);;

let rec zipwith f l1 l2 =
  match l1, l2 with                                                               
  | [], _ -> l2
  | _, [] -> l1
  | hd :: tl, hd2 :: tl2 -> if tl != [] && tl2 != [] then (hd + hd2) :: (zipwith f tl tl2) else (f hd hd2)::[];;

let rec skipElement p l element = 
  match l with
  | [] -> []
  | hd :: tl -> 
      if p hd element 
      then skipElement p tl element 
      else hd::skipElement p tl element;;

let rec keepElement p l element = 
  match l with
  | [] -> []
  | hd :: tl -> 
      if p hd element 
      then hd::keepElement p tl element 
      else keepElement p tl element;;

let rec bucketHelper p l =
  match l with
  | [] -> []
  | hd :: tl -> 
      (keepElement p l hd)::bucketHelper p (skipElement p tl hd);;

let buckets p l =
  bucketHelper p l;;

let rec helperFib prev afterMark iterator n = 
  if iterator = n then prev + afterMark else helperFib (prev + afterMark) prev (iterator + 1) n;;

let fib n =
  if n > 1 then helperFib 1 0 2 n else n;;

let fib_tailrec n =
  fib n;;

let rec sixSkipElement p l element = 
  match l with
  | [] -> []
  | hd :: tl -> 
      if p hd element 
      then sixSkipElement p tl element 
      else hd::sixSkipElement p tl element;;

let rec sixKeepElement p l element = 
  match l with
  | [] -> []
  | hd :: tl -> 
      if p hd element 
      then hd::sixKeepElement p tl element 
      else sixKeepElement p tl element;; 


let rec sixBucketHelper p l =
  match l with
  | [] -> []
  | hd :: tl -> begin
      let miniList = (sixKeepElement p l hd) in
      let miniLength = List.length miniList in
      (hd, miniLength)::sixBucketHelper p (sixSkipElement p tl hd);
    end;;

let assoc_list l =
  sixBucketHelper (=) l;;

let mapFunction func list = 
  List.map func list;;

let rec helper functions agents = 
  match functions with
  | [] -> []
  | hd::tl -> mapFunction hd agents@helper tl agents;;

let ap fs args = 
  helper fs args;;

(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for cond_dup *)
  let _ =
    try
      assert (cond_dup [3;4;5] (fun x -> x mod 2 = 1) = [3;3;4;5;5])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for n_times *)
  let _ =
    try
      assert (n_times((fun x-> x+1), 50, 0) = 50)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for zipwith *)
  let _ =
    try
      assert ([5;7] = (zipwith (+) [1;2;3] [4;5]))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for buckets *)
  let _ =
    try
      assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]);
      assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]);
      assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fib_tailrec *)
  let _ =
    try
      assert (fib_tailrec 50 = 12586269025)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for assoc_list *)
  let _ =
    let y = ["a";"a";"b";"a"] in
    let z = [1;7;7;1;5;2;7;7] in
    let cmp x y = if x < y then (-1) else if x = y then 0 else 1 in
    try
      assert ([("a",3);("b",1)] = List.sort cmp (assoc_list y));
      assert ([(1,2);(2,1);(5,1);(7,4)] = List.sort cmp (assoc_list z));
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for ap *)
  let _ =
    let x = [5;6;7;3] in
    let b = [3] in
    let fs1 = [((+) 2) ; (( * ) 7)] in
    try
      assert  ([7;8;9;5;35;42;49;21] = ap fs1 x);
      assert  ([5;21] = ap fs1 b);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 7 programming questions are incorrect.\n") (!error_count)

let _ = main()
