//// ESERCIZIO 1

// sums è come una sommatoria, che può essere una costante o una somma di sums
type sums = 
    | C of int
    | Add of sums list 

//1) Funzione che conta gli Add in un sums

//2) Funzione che dato un sums sostituisce tutti gli Add [] con C 0

//3) Funzione che calcola il valore di un sums

//4) Funzione che controlla che non ci siano Add[] in un sums

//5) Proprietà che dopo la rimozione con 2) degli Add[], non ve ne siano più


//// ESERCIZIO 2

// L'idea è di modellare un gioco di carte fra due giocatori

// seme rappresenta i semi delle carte da gioco
// card rappresenta una carta, cioè una carta con numero e seme o un jolly
type seme = C | Q | F | P
type card =
    | K of int * seme //(numero, seme)
    | J //jolly

// 1) Funzione che date due carte restistuisce true se la prima vince sull'altra.
// vince se è jolly e l'altra no o se sono dello stesso seme e ha numero più alto.
// vince: c1:carta -> c2:carta -> bool

// 2) Funzione che estrae carte da una lista finchè non trova una carte che vince sulla carta data 
// (prende una carta e una lista di carte), o None se non lo trova (quindi Some carta se lo trova).
// estrai : cs:carta list -> c:carta -> carta option

// 3) Funzione che gioca una partita date due liste di carte, in cui ogni giocatore pesca una carta
// per turno, e si confrontano. Quando uno dei due finisce le carte termina la partita
// restituisce una tripla (vittorie G1, vittorie G2, pareggi) di interi.
// partita : csG1:carta list ->csG2:carta list -> int * int * int

// 4) Funzione analoga a 3) ma che non restituisce nulla, e stampa cosa succede all'evolversi della
// partita, es: "G1: J; G: K(1,Q). Vince G1", e alla fine stampa il risultato.

// 5) Funzione che genera una carta random
// cartaRnd: unit -> carta
open System 
let rnd = Random() //si usa del tipo: let n_casuale = rnd.Next(a,b) e genera un numero intero n tale che a <= n < b

// 6) Funzione che gioca una partita random con k carte
// partitaRnd: k:int -> unit



// N.B.: in questo facsimile mancano i vari test che erano presenti nel testo vero, 
// si lascia al lettore come esercizio la scrittura di simili test ;)