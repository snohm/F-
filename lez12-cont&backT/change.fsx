(*   IMPLEMENTAZIONE  BACKTRACKING
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Possimo usare eccezioni/opzioni/continuazioni per cambiare il flusso
di controllo di un programma e implementare strategie di esecuzione

Esempio: Change Coin Problem (problema del resto)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Input:
 - lista di  tagli di monete disponibili  (coins)
 - quantita' da dare come resto usando le monete disponibili (amount)

Output:
  lista di monete [c1; ...; cn] (resto) tale che c1 + .. + cn = amount;
  ciascuna moneta ck usata nel resto  deve comparire nella lista coins dei tagli disponibili. 

Nella lista  coins e nella lista che rappresena la soluzione, 
l'ordine con cui le monete sono elencate e' irrilevante. 

Per semplicita' assumiamo che, per ciascuna moneta ck che compare nella lista coins,
si hanno a disposizione un numero illimitato di monete di taglia ck.

 Esempio 1
 ---------

coins = [ 1 ; 2 ; 3 ;  7 ]   amount = 5

Una  possibile soluzione e'

[1; 1; 1 ;1; 1]  // cinque monete da 1
 
Altre soluzioni:

 [2; 3]
 [1; 1; 3]
 [1 ; 1 ; 1; 2]
 [1 ; 2 ; 2 ]

Esempio 2
----------
 coins = [ 3 ; 7 ]  amount = 5

Il problema non ammette soluzioni.

 ******

Tentiamo una definizione ricorsive della funzione

  change :  cs:int list -> amt:int -> int list

dove cs e' la lista dei tagli a disposizione (coins) e amt la cifra da cambiare (amount).

Consideriamo la chiamata
   
     change cs amt 


e analizziamo i casi possibili.

Caso 1:  amt = 0 (caso base)
----------------------------

L'unica soluzione e' la lista vuota (non c'e' nulla da cambiare). Quindi:

  change cs 0 = []

Caso 2: cs = [] e amt > 0  (caso base)
-----------------------------------

Il problema non ha soluzione. Quindi:

  change [] amt = FAIL     IF amt > 0

*NOTA* 

Non confondere il fallimento con la lista vuota:
- la lista vuota e' una soluzione al problema;
- il fallimento significa che il problema non ha soluzione

Caso 3: lista cs non vuota e amt > 0 (passo induttivo)
-------------------------------------------------------

Sia  

  cs = c :: xs. 

Distinguiamo due sottocasi.

  3.1 c <= amt

      Utilizzo la moneta c per il resto e continuo cercando una soluzione al problema
      in cui la lista dei tagli e'  cs e amount e' (amt- c)

      change cs amt = c :: ( change cs  (amt - c) )     IF  cs = c :: xs AND  c <= amt

  3.2  c > amt.
   
      La moneta c non puo' essere usata.
      Cerco una soluzione in cui la lista dei tagli e' xs  e la quantita' e' amt

      change cs amt = change xs amt                     IF  cs = c :: xs AND  c > amt

Vedremo piu' avanti che la soluzione descritta *NON* e' corretta
 
---

Definiamo la funzione

   naiveChange: cs: int list -> amt: int -> int list

 che implementa la procedura descritta sopra.
 
 Per rappresentare  il fallimento, introduciamo la eccezione NoChange  

   *)



exception NoChange // fallimento (il problema non ha soluzione)


// naive implementation (*NON* corretta)
// naiveChange: cs: int list -> amt: int -> int list
let rec naiveChange cs amt = 
    match (cs,amt) with
        | (_, 0) -> []                // 1 --  successo
        | ([], _) -> raise NoChange   // 2 --  fallimento
        | (c::xs, amt)  ->
                if c <= amt then  // 3.1
                     c :: naiveChange cs (amt-c)   
                else  // c > amt, 3.2  
                    naiveChange xs amt           

(*

E' possibile dimostrare la terminazione di naiveChange usando l'ordinamento < sulla coppia (cs,amt) 
corrispondente al primo e secondo argomento della funzione

Infatti:

- nella chiamata ricorsiva in 3.1, si ha stesso cs e amt minore
- nella chiamata ricorsiva in 3.2, cs e' diminuito (infatti, xs e' la coda di xs)

*)
 
 // esempio di chiamata
let ch1 = naiveChange [2;1] 4

(*
naiveChange [2;1] 4 =  2 :: naiveChange [2;1] 2 
                    =  2 :: 2 :: naiveChange [2;1] 0 
                    =  2 :: 2:: []
                    =  [2; 2]
              *)

// proviamo un problema non risolubile
let noCh = naiveChange [2] 3
// ....  Exception of type 'FSI_0003+NoChange' was thrown ....

(*
naiveChange [2]  3  =  2 :: naiveChange [2]  1 
                    =  2 :: naiveChange []   1   
                    FAIL
*)

// cosa succede con questo ?

let why = naiveChange [3;2] 4
// ....  Exception of type 'FSI_0003+NoChange' was thrown ....

(* Il problema posto ha soluzione [2;2].

   La funzione naiveChange ha pero' sollevato eccezione NoChange

Analizziamo traccia 

naiveChange [3;2] 4 =  3 :: naiveChange [3;2] 1
                    =  3 :: naiveChange [2] 1
                    =  3 :: naiveChange [] 1
                   FAIL

Problema
^^^^^^^^^
Al primo passo viene scelta la moneta 3. 
Con questa scelta, il successivo  problema da risolvere e'
    
    change [3;2] 1

 che non ha soluzione, e infatti  l'esecuzione fallisce dopo due passi.

Questo pero' non autorizza a concludere che il problema iniziale non e' solubile.

Infatti, e' possibile trovare una soluzione tornando indietro (backtrack) 
al punto iniziale e scegliendo la moneta 2 anziche' 3.
 
Infatto, scegliendo 2, si ottiene

  naiveChange [3;2] 4  = 2 :: naiveChange [3; 2] 2

E' facile vericare che
   
    naiveChange [3; 2] 2 = [2]

 Quindi, con la scelta iniziale di 2 la soluzione e'
     
  2 :: naiveChange [3; 2] 2 =  2 :: [2]  = [2; 2]
     
Notare che il backtracking e' necessario solamente nel caso 3.1. 

Siano

  coins = c :: cs  e  c <= amt

Cerco una soluzione in cui la moneta c e' scelta.
Se si ha fallimento, continuo la ricerca  usando le monete in cs (backtracking).

Nel caso 3.2

   coins = c :: cs   e  c > amt

l'unica scelta possibile e' scartare c, quindi in questo caso non e'  necessario backtracking

======================

Questa strategia di ricerca, in cui a fronte di un fallimento si torna indietro
per tentare, dove possibile, scelte diverse,  si chiama *backtracking*.

Il modo funzionale di realizzare il backtracking e' elegante, ma un po' procedurale 
e non e' immediato da comprendere. 

Analizziamo due possibili inplementazioni, una bastata sull'uso di eccezioni,
l'altro con option types.


In programmazione logica (Twelf, Prolog, ...) il backtracking e' gestito dall'interpete.
Di conseguenza, il programma che risolve il problema puo' essere scritto in termini 
dichiarativi,  e non e' necessario  implementare la strategia di ricerca.

Notiamo anche che il backtracking non e' l'unico modo di esplorare uno
spazio di ricerca: lo si può fare con strategia in "ampiezza"
(breadth-first) e  ottimizzazioni (come A* ) che permettono di
costruire una soluzione "a strati" e quindi con ben maggiore
complessita' computazionale
                
*)




// ** SOLUZIONI CORRETTE (CON BACKTRACKING)  **

// Soluzione 1: backtracking gestito con eccezioni
// change : cs: int list -> amt: int -> int list
let rec change cs amt = 
    match (cs,amt) with
        | (_, 0) -> []                  // successo
        | ([], _) -> raise NoChange    // fallimento
        | (c::xs, amt)  ->
            if c <=  amt then  // caso 3.1 (richiede backtracking)
                try 
                    c :: change cs (amt-c)
                 with
                    | NoChange -> change xs amt // continuo la ricerca con xs e amt
            else  // c > amt   caso 3.2 (non richiede backtracking)
                change xs amt                       



// Esempi
let change1 = change [2;1] 4 // [2; 2]
let noChange = change [4 ; 5  ] 11 // NoChange
// esempio che cpn naiveChange non era corretto
let change2 =  change [3;2] 4  //[2; 2] 

 
// stesso algoritmo di change  ma con stampa della traccia, per vedere cosa succede
// change_tr : cs:int list -> amt:int -> int list
let rec change_tr cs amt = 
    match (cs,amt) with
        | (_, 0) -> []                 // successo
        | ([], _) -> raise NoChange   // fallimento
        | (c::xs, amt)  ->
            if c <=  amt then  // caso 3.1 (richiede backtracking)
                try 
                    (printf "using coin=%d with new amount: %d\n" c (amt-c))
                    c :: change cs (amt-c)
                 with
                    | NoChange -> 
                         ( printf "backtracking on coins=%A and amount=%d \n" xs amt )
                         change xs amt // riparto dalla coda dei tagli e cifra corrente
            else  // c > amt   caso 3.2 (non richiede backtracking)
                ( printf "coin=%d too big for amount=%d, now looking at: %A \n" c amt xs )
                change xs amt                       


 
 
// funzione main per change con traccia
// change_tr_main : cs:int list -> amt:int -> int list
let change_tr_main cs amt = 
    try
        let c = (change_tr cs amt)
        sprintf "Return  change: %A for amount %d given coins %A" c amt cs
    with
        | NoChange -> "cannot give change"

// esempi (eseguirli per vedere la traccia)
let change_tr1 = change_tr_main [3; 2] 4
let change_tr2 = change_tr_main [5] 15
let change_tr3 = change_tr_main [2;3] 10
let change_tr4 = change_tr_main [3;2] 10
let change_tr5 = change_tr_main [5;2] 16
let change_tr6 = change_tr_main [7;3] 11

// stesso algoritmo con options
// In questo caso il fallimento e' rappresentato da None, mentre la soluzione e' contenuta in Some
// change_opt : cs:int list -> amt:int -> int list option
let rec change_opt cs amt = 
    match (cs,amt) with
        | (_, 0) -> Some []    // successo          
        | ([], _) -> None      // fallimento          
        | (c::xs, amt)  ->
            if c <=  amt then // caso 3.1 (richiede backtracking)
                match (change_opt cs (amt-c)) with
                    | None -> change_opt xs amt // fallimento (bactracking)
                    | Some rs -> Some (c :: rs)  
                else  // c > amt  caso 3.2 (non richiede backtracking)
                    change_opt xs amt 
                         


// Esempi
let change_opt1 = change_opt [2;1] 4 // Some [2; 2]
let noChange_opt = change_opt [4 ; 5  ] 11 // None
// esempio che cpn naiveChange non era corretto
let change_opt2 =  change_opt [3;2] 4  // Some [2; 2] 



(*

RICERCA DI TUTTE LE SOLUZIONI
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Le funzioni precedenti si limitano a cercare una soluzione del problema.
Vogliamo ora definire una funzione change_all che trova *tutte* le soluzioni del problema.

Il tipo della funzione e'

  change_all:  int list -> int -> int list list
                 cs        amt   

dove, come prima

cs : int list   lista dei tagli disponibili (coins) 
amt :int        quantita' da cambiare  (amount)

Notare che il risultato e' una lista di soluzioni, dove ciascuna soluzione e' una lista di interi, 
quindi il tipo del risultato e' 'int list list'.

In questo caso, il fallimento del problema (assenza di soluzioni) e' rappresentatato dalla lista vuota.

====

La definizione di change_all non e' semplice.
La soluzione proposta utilizza  CPS (Continuation Passing Style)

Funzione ausiliaria change_all_cps 
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Definiamo la funzione ricorsiva ausiliaria  change_all_cps che,
oltre ai parametri cs e amt, usa gli accumulatori acc e accAll  e la continuazione k.
 

   acc : int list               accumulatore per costruire la soluzione corrente
accAll : int list list          accumulatore per collezionare tutte le soluzioni trovate
    k  : int list list  ->  'a  continuazione   

La continuazione k ha argomento di tipo 'int list list', che rappresenta la lista di soluzioni finora trovate;
intuitivamente, la funzione k continua la ricerca di nuove soluzioni.

       
Il tipo di change_all_cps e' quindi

 change_all_cps :
    int list ->  int ->  int list -> int list list -> (int list list -> 'a)  -> 'a
      cs         amt       acc           accAll                k 
 
Dato che vanno cercate *tutte* le soluzioni, il passo induttivo va modificato nel seguente modo.

Consideriamo la chiamata

   change_all_cps cs amt acc accAll k 

Nei casi base  (amt=0 oppure cs = []) la definizione e' semplice.

Consideriamo ora il caso

  cs = c :: xs AND  amt > 0.

Devo considerare due sottocasi

(i) c <= amt

  In questo caso la moneta c puo' essere o non posso usata;
  poiche' si stanno cercando tutte le soluzioni, vanno considerati  *entrambi* i casi.

  Nella chiamata ricorsiva considero il caso in cui la moneta c viene usata
    
    change_all_cps cs (amt - c) (c :: acc) accAll k1   // k1 e' la nuova continuazione definita sotto
                                 ^^^^^^^^^ 
                                 c e' aggiunto all'accumulatore 
                                 della soluzione corrente

  Il caso in cui la moneta c non e' usata e' ritardato e delegato alla continuazione.
  
   Va quindi definita la nuova continuazione k1 tale che, dato v_accAll
   (parametro che verra' istanziato con la lista delle soluzioni accumulate),
   effettua la chiamata  a change_all_cps dove 
   - la lista delle monete utilizzabili e' xs (c non va usata, uso le rimanenti monete)
   - la quantita' e' amt  (stessa quantita')
   - l'accumulatotre della soluzione corrente e' acc  
     (non si usa c, quindi l'accumulatore della soluzione corrente non cambia)
   - l'accumulatore delle soluzioni e'  v_accAll (parametro che verra' istanziato quando k1 verra' chiamato)
   - la continuazione e' k (continuazione corrente)
  
La definizione esplicita di k1 e':

 let k1 v_accAll =  change_all_cps xs amt acc v_accAll k

   
(ii) c < amt

In questo caso non posso utilizzare c, quindi l'unica soluzione e' quella in cui non si usa c.
 


*)     

// change_all_cps:
//  cs: int list ->
 //   amt: int ->
 //   acc: int list -> accAll: int list list -> k: (int list list -> 'a) -> 'a
let rec change_all_cps cs amt acc accAll k =
    match (cs, amt) with
        | (_, 0) ->  k (acc :: accAll)   // acc e' una nuova soluzione, va aggiunta ad accAll
        | ([], _) -> k accAll            // acc e' scartato perche'  non e' una soluzione  
        | (c :: xs, amt) ->
            if c <= amt 
            then // cerco soluzioni in cui uso c  e poi quelle in cui non uso c 
                let k1 v_accAll =  change_all_cps xs amt acc v_accAll k 
                    // continuation con soluzioni che non usa c
                change_all_cps cs (amt - c) (c :: acc) accAll k1  // soluzione che usa c    
            else // c > amt
                 // c non puo' essere usato nella soluzione corrente
                change_all_cps xs amt acc accAll k    
// Notare che la funzione change_all_cps e' ricorsiva in coda

(*

Osserviamo che nel caso

    cs = c :: xs AND  amt > 0.

la soluzione in cui non si sceglie c va sempre considerata.

Si puo' allora riscrivere il codice in un altro modo:

- si considera come primo caso la soluzione in cui c non e' scelta.
- se c >= amt, si delega alla continuazione la ricerca di una soluzione
  in cui c e' scelta.

 Questo e' fatto nella funzione  change_all_cps1 scritta sotto
*)




// change_all_cps1 (altro modo di definire change_all_cps)
//  cs: int list ->
 //   amt: int ->
 //   acc: int list -> accAll: int list list -> k: (int list list -> 'a) -> 'a
let rec change_all_cps1 cs amt acc accAll k =
    match (cs, amt) with
        | (_, 0) ->  k (acc :: accAll)   // acc e' una nuova soluzione, va aggiunta a accAll
        | ([], _) -> k accAll            // acc e' scartato perche'  non e' una soluzione  
        | (c :: xs, amt) ->
            let k1 =  // definizione  continuazione k1
                if c <= amt then  // k1 cerca  le soluzioni in cui c e' scelta 
                    fun v_accAll ->  change_all_cps1 cs  (amt - c) (c :: acc) v_accAll k 
                else k    // k1 = k  
            change_all_cps1 xs amt acc accAll k1 // c non e' scelta    
 






// funzione principale per trovare tutte le soluzioni del problema
// change_all: cs: int list -> amt: int -> int list list
let  change_all cs amt = change_all_cps cs amt [] [] id 
// accumulatori vuoti, continuation e' id (funzione identita')
// OPPURE
// let  change_all cs amt = change_all_cps1 cs amt [] [] id 

// esempi
let ch_all1 = change_all [ 2; 1 ] 4
//[[1; 1; 1; 1]; [1; 1; 2]; [2; 2]]
let ch_all2 = change_all [ 2 ] 3
// []
let ch_all3 = change_all  [ 3; 2 ] 4
// [[2; 2]]
let ch_all4 = change_all  [ 10; 3; 2; 4 ] 9
//  [[4; 2; 3]; [2; 2; 2; 3]; [3; 3; 3]]

(*

ESERCIZIO
==========

Nelle funzioni scritte sopra, si e' sempre assunto che ciascuna moneta della lista coins
puo' essere usata un numero illimitato di volte.

Riscrivere le funzioni viste assumendo che coins rappresenti la lista delle monete effettivamente disponibili.

Ad esempio se

 coins = [1; 2; 1; 5; 2; 2; 1; 2]

significa che abbiamo a disposizione tre monete da 1, quattro monete da 2 e una moneta da 5. 

Si puo' anche assumere che coins sia una lista di coppie (c,k), dove:
 c indica il taglio di una moneta, k la quantita' disponibile.

 Ad esempio, la lista coins definita sopra puo' essere rappresentata dalla lista di coppie

    [(1,3); (2,4); (5;1) ]
  

*)



// Esempio con generazione casuale:

#r "nuget:FsCheck"
open FsCheck

let top() =
    let coins = Gen.sample 4 4 (Arb.generate<int> |> Gen.filter (fun x -> x > 0))
    let amt = Gen.sample 1 1 (Gen.choose(10,20))  |> List.head
    change_tr_main coins amt
