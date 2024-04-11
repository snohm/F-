#r "nuget:FsCheck";;
open FsCheck
// 1.1
let rec propRmEven xs =
    match xs with
    | [] -> []
    | x :: ys -> if x % 2 = 0 then propRmEven ys else  x :: propRmEven ys 
let rmEven xs = 
    List.filter (fun x -> x % 2 <> 0) xs = propRmEven xs

do Check.Quick rmEven 

// 1.2
let rec rmOddRic xs =
    match xs with
    |  [] -> []
    | [xs] -> [xs]
    | x :: y :: ys -> x :: rmOddRic ys

let rmOdd xs =
    List.length (rmOddRic xs) - (List.length xs) / 2 <= 1  

do Check.Quick rmOdd

//1.3
let rec split xs =
    match xs with
    | [] -> [], []
    | [xs] -> [xs], []
    | x :: y :: ys -> let (sx, dx) = split ys
                      (x :: sx, y :: dx)

let rec merge xs ys = 
    match (xs, ys) with
    | [], [] -> []
    | [xs], [] -> [xs]
    | [], [xs] -> [xs]
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, y :: ys  ->  x :: y :: merge xs ys 
let prop xs = let (a,b) = split xs 
              merge a b = xs

do Check.Quick prop

//
let rec downTo n = 
    match n with
    | 0 -> [0]
    | _ -> n :: downTo (n-1)

let propDownTo n =
    (n >= 0) ==> lazy(downTo n = [n .. -1 .. 0])
    
do Check.Quick propDownTo     