// intro
type figura = 
   | Rettangolo of  float * float      // (base, altezza)
   | Quadrato   of  float              // lato
   | Triangolo  of  float * float      // (base, altezza)
let area fig =
   match fig with
   | Rettangolo(b,h) ->   b * h     
   | Quadrato l      ->   l * l  
   | Triangolo(b,h)  ->  ( b * h )  / 2.0 
let well_formed fig =
   match fig with
   | Rettangolo(b,h) | Triangolo(b,h) ->   b >= 0 && h >= 0     
   | Quadrato l      ->   l >= 0  
//
let areaOpt fig =
    let valid = well_formed(fig)
    match valid with
    | false -> None
    | _ -> Some (area fig)
// 
let printArea fig =
 let x = areaOpt fig
 match x with
 | None -> "la figura non e' ben definita"
 | _ -> "area: " + string(area(fig))
//
let sommaAree f1 f2 =
    let ok1 = well_formed (f1)
    let ok2 = well_formed (f2)
    match (ok1,ok2) with
    | (true,true) -> Some(area (f1) + area (f2))
    | _ -> None 
//
let rec sommaAreaList xs =
    match xs with
    | [] -> 0.
    | x :: ys -> if well_formed (x)
                 then area(x) + sommaAreaList ys
                 else sommaAreaList ys