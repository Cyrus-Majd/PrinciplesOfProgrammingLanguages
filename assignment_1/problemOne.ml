let rec range num1 num2 =
    if num1 > num2 then []
    else num1 :: range (num1+1) num2;;

range 2 5;;
