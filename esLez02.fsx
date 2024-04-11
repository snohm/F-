// esponenziale 
let rec exp a n =
    match n with
    | 0 -> 1
    | _ -> a * exp a (n-1) 
//
let rec makeStr n =
    match n with
    | 0 -> string(0)
    | _ -> makeStr(n-1) + " " + string(n)
// 
let rec mkSumStr n =
    match n with
    | 0 -> (0,"0")
    | _ -> 
        let (a,b) = mkSumStr (n-1)
        (a + n, b + " " + string n)
//
let rec sommaStr n =
    match n with
    | 0 -> ("0 ",0)
    | _ -> let (a,b) = sommaStr(n-1) 
           (string(a) + "+ " + string(n) + " ", b + n)   
let sommaN n =
    let (a,b) = sommaStr(n)
    a + "=" + " " + string(b) 