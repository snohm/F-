// *****************************************//
//              Esercizio 1                 //
// *****************************************//

// sums è come una sommatoria, che può essere una costante o una somma di sums
type sums = 
    | C of int
    | Add of sums list 

//1) Funzione che conta gli Add in un sums

//2) Funzione che dato un sums sostituisce tutti gli Add [] con C 0

//3) Funzione che calcola il valore di un sums

//4) Funzione che controlla che non ci siano Add[] in un sums

//5) Proprietà che dopo la rimozione con 2) degli Add[], non ve ne siano più

let es1 = C 0
let es2 = Add[]
let es2a = Add[C 0]
let es3 = Add[C 1; C 2; C 3]
let es4 = Add[C 1; C 2; Add[C 1; C 2; Add[C 1; C 2; Add[C 1; C 2; Add[C 1; C 2; C 3]]]]]
let es5 = Add[Add[C 1; C 2; C 3]; C 2; Add[C 1; C 2; Add[C 1; C 2; Add[C 1; C 2; Add[C 1; C 2; C 3]]]]]
let es6 = Add[C 0; Add[]]

//1 
let rec countAdd summ = 
    match summ with
    | C _ -> 0
    | Add [] -> 1
    | Add xs -> 1 + List.fold(fun acc x -> acc + countAdd x) 0 xs

//2
let es7 = Add[Add[C 1; C 2; Add[]; C 3; Add[C 5; C 7]]; Add[]; C 4; Add[Add[Add[]]; C 5]]
let es8 = Add[C 1; Add[]; Add[C 2; C 3; C 4]]

//toglie tutti gli add non solo quelli vuoti
let rec remEmptyAdd summ =
    match summ with
    | C x -> [C x]
    | Add[] -> [C 0]
    | Add xs -> List.fold(fun acc x -> acc @ remEmptyAdd x) [] xs 