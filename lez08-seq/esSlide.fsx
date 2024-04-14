(*
Esercizio 2

Usando  Seq.initInfinite definire le seguenti sequenze infinite:

- nat : sequenza dei numeri naturali 0, 1, 2, ...
- nat1: sequenza dei numeri naturali senza il numero 5
- nat2: sequenza dei numeri naturali in cui il numero 5 e' sostituito da -5
- even10 : sequenza dei numeri pari n >= 10  
- sqTrue : sequenza costante true, true, true, ....
- sqTrueFalse: sequenza true, false, true, false, true, false, ...

Per ciascuna sequenza generare la lista dei primi 10 elementi.
*)
let nat = Seq.initInfinite id // (fun x -> x ) 
nat |> Seq.take 10 |> Seq.toList // per mostrare un numero arbitrario della sequenza 
let nat1 = Seq.initInfinite (fun x -> if x < 5 then x else x + 1) 
let nat2 = Seq.initInfinite (fun x -> if x <> 5 then x else -5)
let even10  = Seq.initInfinite ( fun x -> 2 * x + 10 )
let sqTrue = Seq.initInfinite (fun x -> true)
let sqTrueFalse = Seq.initInfinite (fun x -> x % 2 = 0 ) 
(*
Esercizio 3

i) Definire la funzione ricorsiva

    intFrom : int -> seq<int>

che, dato un intero n,  genera la sequenza infinita degli interi
maggiori o uguali a n:
ii) Usando intFrom, definire la sequenza infinita dei numeri naturali 0, 1, 2, ...

iii) Usando intFrom, definire la sequenza infinita nat10  degli interi k >= -10.

iv) Da nat10, usando le funzioni sulle sequenze, estrarre la lista
degli interi compresi fra -4 e 4:
*)
let rec intFrom n = seq{
    yield n
    yield! intFrom(n + 1)
    }
let nat5 = intFrom 0
let nat10 = intFrom -10
nat10 |> Seq.skip 6 |> Seq.take 9 |> Seq.toList

(*
Esercizio 4

Ridefinire le sequenze infinite nat1, nat2, even10, sqTrue, sqTrueFalse usando
sequence expression con ricorsione.
Per nat1, nat2, even10 vanno  definite delle opportune funzioni generatrici (analoghe a intFrom).
*)
// I)
let rec intFrom1 n= seq {
     if n <> 5 then yield n 
     yield! intFrom1( n + 1 )
}
intFrom1 0 |> Seq.take 10 |> Seq.toList
let nat1Ric = intFrom1 0
nat1Ric |> Seq.take 10 |> Seq.toList

// II)
let rec intFrom2 n= seq {
     if n <> 5 then yield n else yield -5
     yield! intFrom2( n + 1 )
}
let nat2Ric = intFrom2 0
nat2Ric |> Seq.take 10 |> Seq.toList

// III)
let rec intFrom3 n = seq{
    if n >= 10  then yield n
    yield! intFrom3 (n + 2) 
}
let even10Ric = intFrom3 0
even10Ric |> Seq.take 10 |> Seq.toList

// IV)
let rec sqTrueRic = seq{
    yield true
    yield! sqTrueRic
}
sqTrueRic |> Seq.take 10 |> Seq.toList

// VI)
let rec sqTrueFalseRic = seq{
    yield true
    yield false
    yield! sqTrueFalseRic
}
sqTrueFalseRic |> Seq.take 10 |> Seq.toList

(*
Esercizio 5

i) Definire la funzione ricorsiva higher-order

    map : ('a -> 'b) -> seq<'a> -> seq<'b>

analoga alla funzione  map su liste in cui si assume che la sequenza sia infinita.
ii)  Applicare map alla sequenza infinita nat dei naturali 
per generare la sequenza infinita squares  dei quadrati dei naturali.
  
Verificare che la lista dei primi 20 elementi di squares e':

[0; 1; 4; 9; 16; 25; 36; 49; 64; 81; 100; 121; 144; 169; 196; 225; 256; 289; 324; 361]
*)
let rec map f sq = 
    let h = Seq.head sq
    let sq = Seq.tail sq    
    seq{
        yield f h
        yield! map f sq
    } 
let square = fun x-> x*x
map square nat |> Seq.take 20 |> Seq.toList;;

(*
Esercizio 6

i) Definire la funzione ricorsiva  higher-order

 filter : ('a -> bool) -> seq<'a> -> seq<'a>

che, dato un predicato pred : 'a -> bool e una sequenza *infinita* sq,
genera la sequenza degli elementi di sq che verificano sq.

ii) Applicare filter alla sequenza infinita nat dei naturali
per generare la sequenza infinita dei multipli di 3 (0, 3, 6, ...)

Verificare che la lista dei primi 20 elementi della sequenza generata e'

 [0; 3; 6; 9; 12; 15; 18; 21; 24; 27; 30; 33; 36; 39; 42; 45; 48; 51; 54; 57]
*)

let rec filter pred sq = 
    let h = Seq.head sq
    let sq = Seq.tail sq
    seq{
     if pred h then yield h
     yield! filter pred sq     
    }
let mut3 x = x % 3 = 0 
filter mut3 nat |> Seq.take 20 |> Seq.toList 

(*
Esercizio 7
i) Definire la funzione
   
 sumSeq : seq<int> -> seq<int>

che, data una sequenza infinita sq di interi
costruisce la sequenza infinita ssq delle somme di sq

ii) Verificare che la lista dei primi 15 elementi della sequenza

    sumSeq nat 
e' [0; 1; 3; 6; 10; 15; 21; 28; 36; 45; 55; 66; 78; 91; 105]
*)
let sumSeq sq =
    let rec sums sq acc= 
        let h = Seq.head sq
        let sq = Seq.tail sq
        seq{
          yield h + acc 
          yield! sums sq (h+acc) 
        }
    sums sq 0 
Seq.toList(Seq.take 10 (sumSeq nat));;
