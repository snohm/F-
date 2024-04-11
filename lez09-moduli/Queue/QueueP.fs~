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


type  Queue<'a> = {front: 'a list; rear: 'a list}



let  empty =  {front = []; rear = []}
// val empty : Queue<'a>


//let put y {front = fs; rear = rs} = {front = fs; rear = y::rs}

// più sintetico
let  put y  q =
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



(*
The core idea has a Part A and a Part B. Part A is: we use the two lists to split the queue into two pieces, the inbox and outbox. When new elements are enqueued, we put them in the inbox. Eventually (we’ll soon come to how) elements are transferred from the inbox to the outbox. When a dequeue is requested, that element is removed from the outbox; or when the front element is requested, we check the outbox for it. For example, if the inbox currently had [3; 4; 5] and the outbox had [1; 2], then the front element would be 1, which is the head of the outbox. Dequeuing would remove that element and leave the inbox with just [2], which is the tail of the outbox. Likewise, enqueuing 6 would make the inbox become [3; 4; 5; 6].
*)

