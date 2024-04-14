//  Motivazione del PBT con FsCheck 
   
// Alcune funzioni standard su liste che abbiamo visto nelle lezioni precedenti

let rec append xs ys = 
    match xs with 
    | [] -> ys 
    | z::zs -> z :: append zs  ys 

let rec rev ls =  
    match ls with 
    | [] -> [] 
    | x :: xs -> append (rev xs) [x]

(* Come è che ci convinciamo che sono corrette? Facciamo qualche (unit) test!  *)

let a1 = append  [ 1 ; 2]  [ 3 ; 4 ; 5 ]  ;; //  [1; 2; 3; 4; 5]

let r1 = rev [1 .. 10] ;; //  [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]

(* A questo punto, ci diciamo "è tipata, ha superato i test (??), andiamo avanti"
 Ma i corner cases?? Esempio artificiale
*)


// nan : float, valore speciale per "not a number". 
    

let r2 = (id nan) = nan

(*
Possiamo fare meglio?

===>  scriviamo delle **proprietà** che il nostro metodo deve soddisfare

- es: rev è involutiva -- una funzione è involutiva se f (f (x)) = x

Nota: Per iniziare, una proprietà è una funzione booleana -- in
seguito introduciamo un semplice DSL per scrivere specifiche.  *)

let prop_revRevIsOrig (xs:int list) =
  rev (rev xs) = xs;;

// val prop_revRevIsOrig : xs:int list -> bool

// A questo punto, faccio degli unit test sulla proprietà

let t1 = prop_revRevIsOrig [1..40]
let t2 = prop_revRevIsOrig []
let t3 = prop_revRevIsOrig [1]

// Che noia! Non possiamo automatizzare? Yes we can -- FsCheck

#r "nuget:FsCheck";;

open FsCheck;;

do Check.Quick prop_revRevIsOrig ;;
// verbosamente

do  Check.Verbose prop_revRevIsOrig ;;

// Che cosa possiamo concludere? Che la nostra funzione "soddisfa" la
// nostra spec (algebra è qui l'oracolo), o meglio che ha superato 100 test 
// Non è una dimostrazione, ma una **validazione**


// Proviamo a scrivere una proprietà **falsa**


let prop_revIsOrig (xs:int list) =
  rev xs = xs
  
do Check.Quick prop_revIsOrig ;;

(*
   Falsifiable, after 2 tests (2 shrinks) (StdGen (470885551,296131751)):
Original:
[-2; 0]
Shrunk:
[1; 0]

Ci dice:

- il controsempio originale, che falsifica la proprietà

- il controsempio ** più piccolo ** (shrinking)  (più dettagli dopo)

- il random seed, nel caso volessi replicare il test

Quindi abbiamo due casi (non mutuamente esclusivi)

1. La proprietà è falsa
2. Il codice è bacato
   *)


// SLIDES !!!


   
// DOMANDE: 
// 1. Tutto così facile? Fscheck risolve la questione della qualità del software?

// 1.1 Ribadiamolo: Non è una dimostrazione, ma una validazione 
// 1.2 Fidarsi bene, non fidarsi ....

let prop_what x =
  x < 100
do Check.Quick prop_what

// ma è ovviamente falso ...

let fff = prop_what 200
// guardiamoci dentro
do Check.Verbose prop_what

// bella forza, siamo attaccati allo zero ... Il punto è che dobbiamo
// essere consapevoli (da subito) che la **distribuzione** dei dati è
// rilevante e FsCheck offre primitive per monitorarla e modificarla


//DOMANDA:  Da dove vengono queste proprietà? Certo, l'algebra è
// un'ispirazione, ma esistono "categorie" o "patterns" di proprietà.
// Lo vedremo in una lezione successiva, intanto
// https://fsharpforfunandprofit.com/posts/property-based-testing-2/


// Altre proprietà? Append è una operazione monoidale, scriviamole insieme ...

// TODO in class

// ha unità
let prop_app_unit (xs : int list) =
    true

do Check.Quick prop_app_unit

// notazione per operatori, per fare prima

let (@@) xs ys = append xs ys
// append è associativo

let prop_assoc (xs : int list, ys,zs) =
 true
do Check.Quick prop_assoc


// E' un monoide commutativo?

let prop_comm (xs :int list, ys) =
    true

do Check.Quick  prop_comm


//  Fino ad ora abbiamo annotato i tipi dei parametri nelle
//  proprietà. Polimorofismo? Viene gestito?


let prop_RevRevp xs =
  rev (rev xs) = xs 
 
// Si, ma non fatelo ...  Potete scrivere properietà polimorfe, ma in realtà
// sono istanziate con 'obj', perché, alla fine, un generatore deve generare dei dati
do Check.Verbose prop_RevRevp;;


// ... e questo "rompe" il type checking poiche' tutto viene cast in obj
// Quindi bisogna renderla monoforma, ma attenzione ....

let prop_RevRevf (xs : float list) =
  rev (rev xs) = xs 

do Check.Quick prop_RevRevf;;

// torna il nan ... Vedremo come escludere questo caso, ma 
// sigifica programmare i generatori, laddove 
// per testare proprietà polimorfe basta instanziare a bool, int

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// SHRINKING

// Ritorniamo a revIsOrig and riguardiamo "shrink":

do Check.Quick  prop_revIsOrig


(* Per capire la ragione del problema è fondamentale trovare
il controesempio più piccolo, laddove la generazione randomica di per se
non lo garantisce.

FsCheck riduce ("shrinks") il controesempio, dopo averlo trovato,
    cercando di eliminare, in questo caso, elementi dalla lista,
    continuando a invalidare la proprietà. Nota che FsCheck
    ricorsivamente riduce anche gli interi, che in questo caso non è
    particolarmente utile.

Non tutte le implementazioni di PBT supportano  shrinking. 

Altri dettagli su shrinking (non entusiasmanti) a
https://fsharpforfunandprofit.com/posts/property-based-testing-1/
*)


(*
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 Testing against a model
 
 Un modo di validare del codice (potenzialmente ottimizzante) è testarlo  contro una
 implementazione modello ("reference"). Eg, testare una vostra
 implementazione di una funzione sulle lsite con quella presente in libreria.
 Se si comportano nello stesso modo, possiamo fidarci.*)


// la mia append "vale" la List.append

let prop_app_mapp (xs :bool list, ys) =
  append xs ys = xs @ ys

do Check.Quick prop_app_mapp

// Utile per validare funzioni che ottimizzano codice dato (e fidato)

// linear reverse
let bfrev xs =
    let rec rev_aux (xs, acc) =
        match xs with 
        | [] -> acc
        | y ::  ys -> rev_aux (ys, y :: acc)
    rev_aux (xs, [])

// le due reverse si comportano allo "stesso" modo
let prop_rev_qrev (xs :int list) =
  bfrev xs = rev xs

do Check.Quick prop_rev_qrev


// Conditional  properties: da bool a Property

(*
Una specifica di una funzione in termini di **contratto** oltre al risultato
della computazione (post-condizione) deve menzionare le assunzioni
secondo le quali la funzione deve essere chiamata correttamente: le pre-condizioni.

In PBT, pre e post condizioni portano a "Properties"  della forma

 <condition> ==> <property>.

Es: testiamo una parte de insertion sort, cioè a funzione insert.

pre: la lista di input list è ordinata
post: il risultato dell'inserimento è ancora una lista ordinata.

In altri termini, essere ordinati è un **invariante** per  <insert>

In primis, definiamo sia il codice che le funzioni che ci servono
per la specifica
 *)


// Lista ordinata
let rec ordered xs = 
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys ->  (x <= y) && ordered ys

// inserimento in lista ordinata 
let rec insert x xs = 
    match xs with
    | [] -> [x]
    | c::_ when x <= c -> x::xs 
    | c::cs ->  c:: insert  x  cs
 
// Invariante: inserimento preserva ordine?  Astriamo sul predicato ordP : int list -> bool
let prop_insert ordP x   (xs : int list)  =
  ordP xs ==> ordP (insert x xs)

do Check.Quick (prop_insert ordered)

let prop_insert2  x   (xs : int list)  =
  ordered xs ==> ordered (insert x xs)

// val prop_insert : x:int * xs:int list -> Property

do Check.Quick (prop_insert2)


// Oops --- FIX THIS !







let rec orderedr xs = 
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys ->  (x <= y) && orderedr (y ::ys)

do Check.Quick (prop_insert orderedr)


// "Arguments exhausted after --- tests."
// Incontriamo per la prima volta l'annoso problema del  **coverage** 

do Check.Verbose (prop_insert orderedr)

(*
Tool genera un certo numero di liste (fino ad un limite prefissato) e
se non superano la pre, vengono scartate.

In questo senso,  ==> è diverso da if-then-else

 *)

 // Soluzione "sbagliata", o meglio poco informativa
let prop_insert_ordered_vacuous (x : int ) xs = 
  if (orderedr xs) then orderedr (insert x xs) else true

do Check.Quick prop_insert_ordered_vacuous

     (* Così ignoro coverage. Se nessuna pre fosse stata soddisfatta, io non ne saprei nulla
e mi illuderei che il test sia stato significativo *)

(* Soluzione corretta:
Ci sono opzioni migliori per codificare la pre-condizione: tipicamente
intervenire sul generatore casuale in modo che internalizza la pre. Qui
generare liste ordinate. Chiediamo a CoPilot e poi vedremo come farlo noi
in prossime lezioni
*)    

// Poco efficiente, ma pazienza 
let orderedListGen = 
    Gen.listOf Arb.generate<int>
    |> Gen.map List.sort

Gen.sample 100 10 orderedListGen

// di nuovo, per i dettagli, me riparliamo

let prop_insert'  x    =
  Prop.forAll  (Arb.fromGen orderedListGen) (fun xs ->
  ordered (insert x  xs))

do Check.Quick prop_insert'

// Problem solved ! Or have we?


// Una definizione sbagliata di insert
let rec bad_insert x xs = 
    match xs with
    | [] -> [x]
    | c::_ when x <= c -> [x;c] 
    | c::cs ->  c:: bad_insert  x  cs

(*
insert 5 [1..10];;
val it: int list = [1; 2; 3; 4; 5; 5; 6; 7; 8; 9; 10]

> bad_insert 5 [1..10];;
val it: int list = [1; 2; 3; 4; 5; 5]
   *)

let prop_insert_bad  x    =
  Prop.forAll  (Arb.fromGen orderedListGen) (fun xs ->
  ordered (bad_insert x  xs))

do Check.Quick prop_insert_bad

// Spec è corretta, ma **non** completa. Deve escludere funzioni che "barano". 


(*
Controlliamo

1. risultato è ordinato
2. il risultato estende la lista di ingresso
3. il risultato contiene x

 *)
let prop_InsertMaintainsOrderAndCompleteness (insertF : int -> int list -> int list) x =
      Prop.forAll  (Arb.fromGen orderedListGen) (fun xs ->
        let newList = insertF x xs
        let isOrdered = ordered newList
        let containsAllOldElements = List.forall (fun elem -> List.contains elem newList) xs
        let containsNewElement = List.contains x newList
        isOrdered && containsAllOldElements && containsNewElement)

// ora la cattiva insert è segnalata
      
let ``Inserting an element into an ordered list maintains order and completeness`` =
   do  Check.Quick (prop_InsertMaintainsOrderAndCompleteness bad_insert)    

// ------------------------------------------------------

// condizioni e  lazyness  (IMPORTANTE)
   
// il primo elemento di una lista ordinata è il minimo
let prop_fsm (xs : int list) =
  List.sort xs |>  List.head = List.min xs

// Ricordate operatore "|>" (pipe)  "e |> f" signifcante (f e)

// qui significa: List.head (List.sort xs)

do Check.Quick prop_fsm


// Ah, la lista vuota ... Secondo tentativo:


let prop_fsm2 (xs : int list) =
  not (List.isEmpty xs) ==>  (List.sort xs |>  List.head = List.min xs)

do Check.Quick prop_fsm2

//  **lazy evaluation**
// uso la keyword 'lazy', per ritardare la valutazione del RHS (notate parentesi)
// in modo che l'antecedente venga valutato prima e filtri la generazione di xs

let prop_fsm3 (xs : int list) =
    not (List.isEmpty xs) 
    ==>
    lazy (List.sort xs |>  List.head = List.min xs)

do Check.Quick prop_fsm3


// ESERCIZIO: scrivere una proprietà simile per List.max


// Back to slides for a second and conclude

// -----------------------------------------------------
   
// Argomenti addizionali non coperti in questa lezione:


// "cross-unit testing": testiamo una proprietà che collega rev e append

let prop_rev_app (xs : bool list, ys) = 
    rev (xs  @@ xs) =  (rev xs) @@ (rev ys)

do Check.Quick prop_rev_app


// In generale, posso usare back ticks per nomi più significativi in
// una let declaration e in particolare:
let ``reversing a list yields the same list``  (xs:int list) =
  rev xs = xs
do Check.Quick ``reversing a list yields the same list``




// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// Combining checks. 

let add(x,y) = x + y 

// use a list of specs for a function <f> for a commutative monoid
// adding a flag to have an idea of what can go wrong. Note the we
// pass <f> as an argument

let prop_op_spec f (x: int, y, z) =
    [f(x,y) = f (y, x)    |@ "comm";
     f (x , f (y, z)) = f (f(x,y), z)    |@ "assoc";
     f (x, 0) = x |@ "leftId";
     f (0, x) = x |@ "RightId"
    ]

// we pass add to the prop
do Check.Quick (prop_op_spec add)

// Let's go crazy

let add2(x,y) = x - y

do Check.Quick (prop_op_spec add2)


// ci dice il primo caso dove fallisce


// Condizioni possono essere composte

let prop_addIsNotMult (x, y) =
    ((x,y) <> (0,0) && (x,y) <> (2,2))    ==> (x + y <> x * y )
     
do Check.Quick prop_addIsNotMult
