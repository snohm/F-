// 1
let rec rmEven xs =
    match xs with
    | y :: ys -> if (y%2) = 0 then rmEven ys else y :: rmEven ys
    | [] -> []
// 2
let rec rmOddPos xs =
    match xs with
    | [] -> []
    | [xs] -> [xs]
    | x :: y :: ys -> x :: rmOddPos ys
// 3
let rec split xs =
    match xs with
    | [] -> [],[]
    | [x] -> [x],[]
    | x :: y :: ys -> let (ps,ds) = split ys
                      (x :: ps, y :: ds)
// 4
let rec swap xs =
    match xs with
    | [] -> []
    | [xs] -> [xs]
    | x :: y :: ys -> y :: x :: swap ys
// 5 
let rec lenList xs =
    match xs with
    | [] -> 0
    | _ :: ys -> 1 + lenList ys
let cmpLength xs ys =
    let lens1 = lenList(xs)
    let lens2 = lenList(ys)
    match (lens1,lens2) with
    | lens1,lens2 when lens1 > lens2 -> 1
    | lens1,lens2 when lens1 = lens2 -> 0
    | lens1,lens2 when lens1 < lens2 -> -1
    | _ -> 2 // se no da PM incompleto
    //if lens1 = lens2 then 0
    //elif lens1 > lens2 then 1
    //else -1  
// 6 
let rec remove x xs =
    match xs with
    | [] -> []
    | y :: ys -> if x = y then remove x ys else y :: remove x ys
// 7 
let rec find x xs =
    match xs with
    | [] -> false
    | y :: ys -> if y = x then true else find x ys
let rec removeDup xs = 
    match xs with
    | [] -> []
    | y :: ys -> if find y ys 
                    then
                        let ris = remove y ys 
                        y :: removeDup ris
                    else 
                        y :: removeDup ys
// 8 
let rec downTo n =
    match n with 
    | 0 -> [0]
    | _ -> n :: downTo (n-1)
let rec upto n =
    match n with 
    | 0 -> [0]
    | _ -> upto(n-1) @ [n]