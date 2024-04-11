// Paulson's implementation 

(*

Una coda qs e' rappresentata da una coppia di liste front e rear:

- front e' la lista dei primi elementi della coda
- rear  e' la lista dei rimanenti elementi della coda, elencati pero' in ordine inverso.

Una coda ammette in generale piu' rappresentazioni.
Ad esempio, la coda

   a ; b ; c           // a e' il primo elemento della coda, c l'ultimo

ammette  quattro rappresentazioni:

 front = [a ; b ; c]   rear = []

 front = [a ; b]       rear = [c]

 front = [a]           rear = [c ; b]

 front = []            rear = [c ; b ; a]

L'operazione put puo' essere implementata in modo efficiente;
infatti, basta aggiungere un elemento in testa alla lista rear

Ad esempio, se la coda  

   a ; b ; c

e' rappresentata come

front = [a ; b]    rear = [c]

aggiungendo l'elemento d si ottiene

front = [a ; b]    rear = [d; c]
 
che rappresenta la coda

 a ; b ; c ; d


L'operazione get si implementa prendendo il primo elemento di front.
Occorre prestare attenzione al caso in cui la lista front sia vuota.
Ad esempio supponiamo di voler eseguire get sulla coda 

   a ; b ; c 

e che la coda sia rappresentata come
 
 front = []                rear = [c ; b ; a]   // (#)

L'elemento da togliere e' l'ultimo della lista rear;
se occorre successivamente togliere altri elementi,
occorre sempre accedere alla coda di rear, e questo e' poco efficiente.

Conviene travasare tutti gli elementi da rear e front;
ovviamente, poiche' la coda deve essere la stessa,
gli elementi vanno posti in ordine inverso:

 front = [a; b; c]          rear = []          // (##)
 
Notare che (#) e (##) rappresentano la stessa coda.
Il vantaggio che si ottiene dopo questa operazione e' che
la lista front non e' piu' vuota, e get e' implementabile in modo efficiente.

   *)

module Queue

exception  EmptyQueue

// Nota sintassi dei record

type  Queue<'a> = {front: 'a list; rear: 'a list}


let  empty =  {front = []; rear = []}
// val empty : Queue<'a>


// sintassi prolissa
let putL y {front = fs; rear = rs} =
    {front = fs; rear = y::rs}

// più sintetico usando dot-notation

let put y q =
    {q with rear = y::q.rear}

// get
// va tolto il primo elemento di front, trattando a parte il caso in cui front e' vuoto  

let rec  get  q =
    match q with
        | {front = [] ; rear = []} -> raise EmptyQueue  // coda vuota
        | {front = x::fs ; rear = rs} as q1 -> (x,{q1 with front = fs})
        | {front = [] ; rear = rs} ->
            // notare che la lista rear non puo' essere vuota
                 get {front = List.rev rs; rear = []}



