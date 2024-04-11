
//#r "QueueN.dll"
// #r "QueueP.dll"

open Queue

(*

a) Definire la coda q1 di interi  ottenuta ponendo nella coda vuota
gli elementi 3, 5, 10

*)

let q1 = put 3 empty |> put 5 |> put 10



(*
b) Estrarre un elemento da q1 e sia q2 la coda ottenuta.
Notare che l'elemento estratto  e' 3

*)

let (x1,q2) = get q1
// val x1 : int = 3

(*

c) Estrarre un elemento da q2 e sia q3 la coda ottenuta.
Notare che l'elemento estratto  e' 5

*)

let (x2,q3) = get q2
// val x2 : int = 5

(*

c) Aggiungere alla coda q3  gli elementi 15, 20 e sia q4 la coda ottenuta.
Estrarre tre elementi da q3 e sia q5 la coda ottenuta.
Notare che gli elementi estratti sono 10, 15 e  20.
Verificare che q5 e' la coda vuota (provare a estrarre elemento da q5)



*)


let q4 = q3 |> put 15 |>  put 20

let (y1, q4_1) = get q4
// val y1 : 10


let (y2, q4_2) = get q4_1
// val y2 : 15

let (y3, q5) = get q4_2
// val y3 : 20


let qs =  put "3" empty
