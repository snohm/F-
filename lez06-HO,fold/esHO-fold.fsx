//        **  HO  **       //

// map
let sqList n = List.map (fun x -> x*x) [1 .. n]
let len xs = List.map (fun x -> String.length(x) < 10) xs
// filter
let pari n = List.filter (fun x -> x % 2 = 0) [0..n]

let mult k m = List.filter (fun x -> x % k = 0 ) [k .. m]

// exists 
let contains x xs = List.exists (fun y -> x = y) xs

let isSquare x = List.exists (fun y -> y * y = x) [0 .. x/2 + 1]

// forall
let allEqual x xs = List.forall (fun y -> x = y) xs   

// concat
List.concat [["1";"5"]; ["6";"10"]]
    (*prende lista di liste e concatena nell'ordine nel 
     quale vnegono passate. Permette di fare cose con le
     sequenze quindi da approfondire List.concat : seq<'a list> -> 'a list
    *) 

// definire le funz HO di cui sopra in modo ricorsivo

//map
let rec map xs =
    let f = (fun x -> x * x )
    match xs with
    | [] -> []
    | x :: ys -> f x :: map (ys)
//filter
let rec filter xs =
    let f = (fun x -> x % 2 = 0)
    match xs with
    | [] -> []
    | x :: ys -> if f x then x :: filter ys else filter ys 
//exists
let rec exists x xs =
    let f = (fun z -> x = z)
    match xs with
    | [] -> false
    | y :: ys ->  if f y then true else exists x ys  

//forall
let rec forall x xs =
    let f = (fun z -> x = z)
    match xs with
    | [] -> true
    | y :: ys ->  if f y then forall x ys else false


//        **  Fold  **        //
// Ridefiniamo le funzioni sumlist, prodlist, lenlist e append con foldback

// sumlist 
let sumlist xs = List.foldBack (fun x acc-> x + acc) xs 0

// prodlist
let prodlist xs = List.foldBack (fun x acc-> x * acc) xs 1

// lenlist 
let lenlist xs = List.foldBack (fun _ acc-> 1 + acc) xs 0
let rec lenlistric xs =
    match xs with
    | [] -> 0
    | _ :: ys -> 1 + lenlistric ys

// append
let append xs ys = List.foldBack (fun x acc -> x :: acc) xs ys
