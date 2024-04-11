// ***  INTRODUZIONE ALLE FUNZIONI DI ORDINE SUPERIORE (HIGHER-ORDER)  ***

(*

Sul libro di testo [HR]:  2.7, 2.8, 2.11.




  ESPRESSIONI FUNZIONALI 



Tipi funzionali
^^^^^^^^^^^^^^^

Dati due tipi T1 e T2

   T1 -> T2     

denota  il tipo delle funzioni da T1 (dominio)  a T2 (codominio).
Il tipo T1 -> T2 e' detto *tipo funzionale*.    

L'operatore -> usato per costruire tipi funzionali:

-  Ha priorita' piu' bassa dell'operatore  * (prodotto cartesiano):

   int * string ->  float     equivale a    (int * string) ->  float

   int -> int * int           equivale a     int -> (int * int)


-  E' associativo a destra:

    int -> int -> int     equivale a     int -> ( int -> int )


Pertanto il tipo  int -> int -> int  e' diverso da (int -> int) -> int:


- int -> int -> int:
  
  tipo di una funzione che, applicata a un int, produce una funzione di tipo int->int

- (int -> int) -> int:

  tipo di una funzione che, applicata a una funzione di tipo int->int, produce un int

Funzioni che hanno come argomento/risultato altre funzioni sono dette di *ordine superiore* (*higher-order*).


Espressioni   funzionali
^^^^^^^^^^^^^^^^^^^^^^^^

Una  *espressione funzionale*  e' una espressione avente tipo funzionale.
   
E' possibile definire espressioni funzionali senza assegnare loro un nome; in tal caso si parla di  *funzioni anonime*.

Esempio, l'espressione funzionale 

    fun x -> x + 1  

definisce una funzione anonima di tipo int -> int che calcola il successore di x.

Vedi in Go

func main() {
    // Declare and immediately invoke an anonymous function
    result := func(x int) int {
        return x + 1
    }(5)


Applicazione di una funzione
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Data una espressione funzionale fExpr e un termine t aventi tipo

   fExpr : T1 -> T2      t : T1

l'espressione

   fExpr t
 
denota la *applicazione*  di fExpr al termine t.
L'espressione  'fExpr t'  ha tipo   T2.

Notare che:

-  A differenza dei linguaggi imperativi, non occorre scrivere l'argomento tra parentesi tonde;
   e' sufficiente separare funzione e argomento da uno o piu' spazi.

-  Se si e' in presenza di tipi polimorfi (tipi che contengono variabili),
   l'applicazione e' possibile solo se il tipo del dominio della funzione
   puo' essere instanziato in modo da essere uguale a quello dell'argomento.

   Ad esempio se 

       f :  'a * 'b ->  ('b  * 'a) list         t  :  int * string

      
    l'applicazione 

          f t 

    e' corretta in quanto con l'instanziazione

         'a = int          'b = string

    il tipo del dominio di f e il tipo di t sono uguali  (tipo  int * string).
    L'espressione 'f t' ha quindi tipo '(dtring * int) list'.

*)
// esempio di funzione f avente tipo 'a * 'b -> (b' *'a)  list
let f (x,y) = [ (y,x) ; (y,x)  ] 

// esempi di applicazione di f

f (3, "tre") 
// val it: (string * int) list = [("tre", 3); ("tre", 3)]

f ( "succ" ,  fun x -> x + 1 )  
// 'a e' istanziato con tipo string, 'b con il tipo int -> int
// val it: ((int -> int) * string) list =
//  [(<fun:it@122-3>, "succ"); (<fun:it@122-3>, "succ")]


(*
L'argomento di una funzione e' valutato *prima* di applicare la funzione.

Ad esempio, la valutazione

   ( fun x -> 10 ) 4/0 
   
non da' risultato 10, ma solleva una eccezione,
dovuta alla valutazione dell'argomento 4/0.

Questa modalita' di valutazione e' chiamata *eager evaluation*.

Al contrario, con la valutazione *lazy* il valore di una espressione
richiede in genere solo una valutazione parziale delle sue  sottoespressioni.

Esempi di operatori che vengono valutati in modalita' lazy sono
gli operatori booleani && (and) e || (or).

  ( 2 = 3 )  &&  (4/0 > 0)   // false

Poiche' 2 = 3 e' false, il valore dell'espressione e' false 
e la sottoespressione '4/0 > 0', che solleverebbe una eccezione,
non e' valutata.
  
  ( 2 < 3 )  ||  (4/0 > 0)   // true

Anche in questo caso, viene valutata solamente la sottoespressione '2 < 3'.

Le valutazioni delle espressioni

(4/0 > 0) &&  ( 2 = 3 )        (4/0 > 0)  || ( 2 < 3 )     

sollevano eccezioni.
Notare che, a differenza degli operatori logic AND e OR,
gli operatori && e  || non sono commutativi.                     
Infatti, la valutazione delle espressioni


( 2 < 3 )  ||  (4/0 > 0)             (4/0 > 0)  || ( 2 < 3 )     

produce risultati diversi (true nel primo caso, errore nel secondo)


Una espressione funzionale e' un *valore (value)* e  puo' essere legata
a un identificatore (let-binding).
Ad esempio, dopo la definizione

  let f = fun x -> expr  // (1)

l'identificatore f  e' legato all'espressione  'fun x -> expr'

Sintassi  equivalente:

 let f x = expr  // (2)

La definizione (2), in cui la x e' portata a sinistra di =, e'  equivalente a (1)
ed e' la notazione generalmente usata.

Questo procedimento  e' parte di una tecnica di compilazione nota come *lambda lifting*
 
*)   

// Esempi

fun x -> x + 1   // funzione anonima  (funzione successore)
//val it : int -> int 

(fun x -> x + 1) (5 + 1 )   
  //  val it : int = 7

// let binding

let succ =  fun x -> x + 1 
// val succ : int -> int
// nell'ambiente corrente, l'identificatore succ e' legato alla funzione 'fun x -> x + 1' 

let ss = succ 4
// val ss : int = 5

// Definizione di succ usando lifting (identificatore x e' scritto a sinistra di =)

let succ1 x =   x + 1 


// funzione identita'

let id = fun x -> x 

// esempio di funzione costante

let zero = fun x -> 0 
//val zero : 'a -> int


(**


 In una espressione funzionale

    fun x -> expr

l'espressione expr puo' a sua volta essere una espressione funzionale. 

Esempio:

    fun x -> ( fun y ->  x + y ) 


Definiamo:

   let f = fun x -> ( fun y ->  x + y ) 

Che tipo ha f ?

Anzitutto, l'operatore '+' viene interpretato come la somma intera;
agli identificatori x e y e' quindi assegnato il tipo int.

Segue che:

    fun y -> x + y                     ha tipo    int -> int

    fun x -> ( fun y ->  x + y )       ha tipo    int -> ( int -> int ) 

Dato che f ha tipo int -> (int -> int) , possiamo applicare f a un qualunque
termine di tipo int e il risultato ha tipo int -> int.
Quindi f e' una funzione che applicata a un valore di tipo int,
restituisce una funzione di tipo int -> int

   
Esempi di applicazione 
^^^^^^^^^^^^^^^^^^^^^^

Applichiamo f a 5. Il risultato deve essere una funzione di tipo int -> int.

Il valore di f 5 e'  ottenuto valutando l'espressione

  fun y ->  x + y  // espressione a destra di -> nella def. di f

nell'ambiente provvisorio in cui e' definito il legame 'x --> 5'

Quindi:
  
  f 5  =   fun y -> 5 + y   // funzione  di tipo int -> int 
                            // rappresenta la funzione che, dato argomento y, calcolo 5 + y

Definiamo:

    let g =  f 5 

Allora g  e' la funzione

    fun y -> 5 + y : int -> int

Esempio di applicazione di g:

g 4  =  ( fun y -> 5 + y ) 4 
     =  9

E' possibile eseguire le due applicazioni scrivendo un'unica espressione:

     (f 5) 4     //  val it : int = 9  
   
Convenzioni sulla associativita'
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Abbiamo visto che  l'operatore ->  e' associativo a destra

L'applicazione di funzione e' invece associativa a sinistra.


     fun x ->  fun y  ->  expr       equivale a        fun x -> ( fun y  ->  expr ) 

                  fExpr t1 t2        equivale a       ( fExpr t1 ) t2
    
     fun x ->  fun y ->  x + y       equivale a        fun x -> ( fun y ->  x + y )

                         f 5 4       equivale a        (f 5) 4

Notare che 'f 5 4' assomiglia all'applicazione di una funzione con due argomenti.
In realta' corrisponde a due applicazioni annidate.

 
  
Sintassi alternativa
^^^^^^^^^^^^^^^^^^^^
 
Quando si hanno piu' 'fun' annidati, si puo' usare una notazione piu' compatta:

     fun x1 ->  ( fun x2 -> .... -> ( fun xn expr) ... )

si puo' riscrivere come

     fun x1 x2 .... xn -> expr   // un solo fun con elenco variabili

Il 'lifting' si estende a espressioni funzionali con piu' variabili:

   let fn =  fun x1 x2 .... xn -> expr

si puo' riscrivere portando gli identificatori x1, x2, ... , xn a sinistra di = :
  
    let fn x1 x2 ... xn = expr

La seguenti definizioni sono equivalenti  

     let f  =  fun x -> ( fun y ->  x + y )  // le parentesi si possono omettere
     let f  =  fun x y -> x + y
     let f x y  =  x + y 

L'ultima definizione assomiglia alla definizione di una funzione con due parametri x e y.

Questo meccanismo per cui si puo' simulare una funzione a n argomenti
mediante funzioni a un argomento annidate si chiama *currying*
(Schoefinkel, Curry, 1930) 	       	  

Quando una funzione f a n variabili (nel senso sopra spiegato) e' applicata applicando k < n argomenti,
si parla di *applicazione parziale*
 	       	  
Notare che nei linguaggi imperativi non e' possibile applicare parzialmente
una funzione (tutti i parametri della funzione vanno istanziati simultaneamente).

  
*)
// Esempi di applicazione parziale

(*
 La funzione 
  
    makeListGen: 'a ->  int -> 'a list

crea una lista contenente l'elemento c (primo parametro) replicato n volte  (secondo parametro)
Si assume n >= 0
*)
let rec makeListGen c n = if n = 0 then [] else c :: makeListGen c (n-1) 

// La funzione  makeList0 : int -> 'a list crea una lista contenente n volte il numero 0; si assume n >= 0
let makeList0  = makeListGen 0
makeList0 5                                                                      
// val it: int list = [0; 0; 0; 0; 0]


// Altri esempi di applicazione parziale

(*
Per nominare la funzione descritta da un operatore, l'operatore va scritto fra parentesi tonde
*)

(=)  // funzione associata all'operatore =

(*
  (=)   e' la funzione   
  
     fun x y -> x = y   // dato x e y, viene valutato se x e' uguale a y
     
 di tipo

 'a -> 'a -> bool   when 'a : equality 

*)

(=) 0  // applicazione parziale
// val it : (int -> bool) 

(*
  (=) 0     e' la funzione  
  
    fun y ->  ( 0 = y) : int -> bool 

Si ha:

 ((=) 0) y  =  true    SE  l'espressione booleana (0 = y) e' vera
               false   ALTRIMENTI 

Quindi '(=) 0' e' la funzione tale che:

    '(=) 0' y  = true IFF y = 0

ossia:

   '(=) 0'  e' la funzione che controlla se y vale 0 (che possiamo chiamare isZero).    

*)   

let isZero = (=) 0 
// val isZero : int -> bool

isZero 0  // true
isZero -1  // false
let succ2 =  fun x -> x + 1 

(*
Definiamo mediante valutazione parziale la funzione

  isPositive : int -> bool

tale che:

   isPositive x =  true   SE  x > 0
                   false  ALTRIMENTI
*)


let isPositive = (<) 0
// val isPositive : int -> bool

(*

 (<)     e' la funzione  fun x y -> (x < y) : ('a -> 'a -> bool) 
                    
 (<) 0   e' la funzione    fun y -> (0 < y) : int -> bool 

*)   

isPositive 10   // true
isPositive -1   // false

(* La funzione succ si puo' definire per applicazione parziale:

   let  succ= (+) 1

   Definiamo

   let h =  (-) 1
  
   Che funzione e' h ? 

*)


(***

Ancora sulla notazione delle espressioni funzionali
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finora abbiamo visto espressioni funzionali della forma

    fun x -> expr

E' anche possibile scrivere a sinistra di -> un termine strutturato (un pattern)

Esempio
^^^^^^^

   fun (x,y) ->  x + y

e' una espressione funzionale di tipo

   int * int -> int

che definisce la funzione che calcola la somma di una coppia di interi.

Usando la sintassi primitica (che ammette solo identificatori a sinistra di ->),
l'espressione precedente si scrive:
  
  fun p -> let (x,y) = p in x + y 
  // funzione che a p associa x + y, dove p = (x,y)


La definizione

 let somma = fun (x,y) ->  x + y  

si puo' riscrivere come (lifting):

 let somma (x,y) = x + y 

Notare la differenza fra

   let  somma (x,y) =  x + y    // somma : int * int -> int
   let      f x y   =  x + y    //     f : int -> int -> int

Per calcolare la somma 1 + 2:

 somma  : ha  come unico argomento la coppia (1,2)
          
    f  : il calcolo di 1 + 2 si ottiene con la chiamata

         ( f 1 ) 2   // applico f a 1 e poi applico il risultayo (che e' una funzione) a 2
*)



(****   COMPOSIZIONE DI FUNZIONI  *****

In programmazione funzionale, una delle operazioni fondamentali fra funzioni e' la loro composizione.


Date due funzioni 

  f : 'a -> 'b      g : 'b -> 'c

l'operatore  operatore  >>  compone le funzioni f e g come segue:

  f >> g  =  fun x ->  g ( f x )

Quindi:
- prima si applica f a x
- al risultato ottenuto si applica g.

 x  |--f-->  f x  |--g-->  g( f x )

Il tipo di  >>  e':

      (>>)  :  ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
   


Esempio
^^^^^^^

Definire la funzione
  
  squareLast : int list -> int

che data una lista xs di interi non vuota, calcola il quadrato dell'ultimo elemento di xs.
Si *assume* che xs non sia vuota.

Possiamo  definire squareLast come composizione delle funzion 

            square   :  int -> int       // quadrato
          List.last  : 'a list -> 'a     // ultimo elemento di una lista
          
Attenzione ad applicare le funzioni nell'ordine giusto.
***)

// square: x: int -> int
let square x = x * x

// squareLast : xs:int list -> int
// Si assume xs non vuoto
let squareLast  = ( List.last >>  square )  // notare l'uso delle parentesi (se tolte, si ha errore)

// equivale a:
// let  squareLast xs =  square  ( List.last xs ) 


(*

NOTA
^^^^
Uno dei problemi dell'uso della composizione e' scrivere le funzioni da comporre nell'ordine giusto.

La confusione deriva dal fatto che F# usa una convenzione diversa rispetto a quella usata in matematica e
adottata da altri linguaggi funzionali.

In matematica, la composizione fra due funzioni (denotata da .) e' definita in questo modo:

 (f . g) x = f (g x)

Quindi, f e g sono scritto in ordine inverso rispetto alla loro applicazione.
Usando questa notazione, la funzione squareLast va scritta come

 squareLast = square . List.last

In F# invece le funzioni sono scritte nell'ordine in cui sono applicate (prima List.last e poi square);
per sottolineare questa differenza, F# introduce un sinbolo diverso per denotare composizione 
(  >>  invece di . )

*)



(****    PIPE    ****

Diversamente da altri linguaggi funzionali, in F# la composizione si usa poco perche'
F# ha introdotto un modo piu' comodo per comporre funzioni, ossia l'operatore |> (pipe).

L'operatore pipe permette di scrivere l'argomento di una funzione prima della funzione stessa.
Quindi, 

    t |> f  
    
denota la applicazione della funzione f al termine t (ossia,   f t ).


Il tipo della funzione (|>) associata all'operatore pipe e':

  (|>) :  'a -> ('a -> 'b) -> 'b

L'uso di pipe e' utile quando occorre applicare funzioni in cascata,
si trova anche in altri linguaggi.

Esempio di pipe nel linguaggio di shell ( il simbolo per pipe  e' la barra | ):

 cat file.txt | sort | tail -1

Vengono eseguiti in successione i comandi cat (stampa file), sort (ordina linee) e 'tail -1' (stampa ultima linea)


*)

// Riscriviamo la funzione squareLast usando pipe
// squareLastPipe : xs:int list -> int
// Si *assume* che  xs sia non vuoto
let squareLastPipe xs = xs |> List.last |>  square

