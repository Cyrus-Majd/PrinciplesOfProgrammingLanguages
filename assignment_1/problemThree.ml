let rec print_list_int myList =
        match myList with
        | [] -> print_endline "end of it"
        | head :: body -> begin
                print_int head;
                print_endline "";
                print_list_int body
        end;;

let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;

let number = [1;2;3;4;5];;
print_list_int number;;
compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
