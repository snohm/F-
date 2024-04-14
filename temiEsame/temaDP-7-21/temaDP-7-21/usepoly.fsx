#r "poly.dll"

open Poly


let p1 = ofList [2; 0; 0; 1]  // p1 = 2 + x^3 

let p2 = ofList [9; 5; 0; -7 ; 0; 0] // p2 = 9 + 5x + (-7)x^3 

let l2 = toList p2  //  [9; 5; 0; -7]

let p1x4 = multxc p1 4 |> toList //  [8; 0; 0; 4]

let p2x5 = multxc p2 5 |> toList //  [45; 25; 0; -35]

let p1xx = multxx p1  |> toList //  [0; 2; 0; 0; 1]

let p2xx = multxx p2  |> toList // [0; 9; 5; 0; -7]

let s = somma p1 p2 |> toList // [11; 5; 0; -6]

let o = somma p1 p2 |> opposto |> toList //  [-11; -5; 0; 6]

let dd =  diff p1 p1|> toList  // []

let g = grado p1 // 3

