let rec elem x a =
  match a with
  | [] -> false
  | h::t -> if h = x then true else elem x t;;

let rec subset a b =
  match a with
  | [] -> true
  | h::t -> if elem h b then subset t b else false;;

let rec remove x a =
  match a with
  | [] -> []
  | h::t -> if x = h then remove x t else h::remove x t;;

let rec unionTest listOne listTwo =
  match listOne with
  | [] -> true
  | h::t -> if elem h listTwo then begin
	(*unionTest t listTwo*)
	end
	else begin
	(* if it aint in the second list *)
	print_int h;
	print_endline "found something";
	(*unionTest t h::listTwo*)
	end;;

let firstList = [2;5];;
let secondList = [1;3;4];;

unionTest firstList secondList;;
