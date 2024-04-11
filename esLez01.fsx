let Or x y =
    match (x,y) with
    | (false, false) -> false
    | _ -> true
let isPari n =
    match (n % 2) with
    | 0 -> string(n)+" e' un numero pari"
    | _ -> string(n)+" e' un numero dispari"
let isPositive n = n >= 0