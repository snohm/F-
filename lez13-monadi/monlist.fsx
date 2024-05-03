// https://fsharpforfunandprofit.com/posts/computation-expressions-builder-part1/


(* What are the "monadic" functions for lists?

List.map :  (('a -> 'b) -> 'a list -> 'b list)
Bind is      List.collect   : (('a -> 'b list) -> 'a list -> 'b list)
Return is  List.singleton;;
Apply : there isn't, but easy to define
          *)


// You have seen sequences: the seq constructor, for, yield, yield! and
// sequence expressions. What is the magic?

// Recall sequence expressions:
let ss = seq {for x in seq {1..10} do x+x};; 

// Seq expressions works also for (eager) lists 
let xs  =  [for x in  [1..10] do x+x];; 

// Sequences are just computational expressions out of lazy lists. 
// <yield> is return, <for> is bind

// We could spell this out in details (as done in the HR book)
// but let's just give an impression

// How come  the library function <collect> is a bind?
// List.collect :  (('a -> 'b list) -> 'a list -> 'b list)
// look at the type and compare with Option.bind

let xs' = List.collect (fun x -> [x + x]) [1..10];;

// So, we can use collect as a bind.
// let's redo lists as monads

type ListBuilder() = 
  member x.Bind(v, f) = List.collect f v
  member x.Return(v) = [v]


let lst =  ListBuilder()

// we get the list expression  as follows
let lexp  =
    lst{
        let! x = [1..10]
        return (x + x)
    }

// what about nested loop?
let l2 =
    [for x in [1..5] do
     for a in ['a'..'e'] do (x,a)]
           
// sure
let lexp2  =
    lst{
        let! x = [1..5]
        let! a = ['a'..'e']
        return (x,a)
    }    

// if you really want the for/yield syntax, you can, by just adding
// the same functions with different member:


type ListWorkflowBuilder() =

    member this.Bind(list, f) =
        List.collect f list

    member this.Return(x) =
        [x]

    member this.For(list, f) =
            this.Bind(list, f)  
    member this.ReturnFrom (m : list<'a>)  = m  

    member this.Zero() = []
    member this.Delay(f) = f() // just a stump
    member this.Combine(x,y) = x @ y
let listWorkflow = ListWorkflowBuilder()

let l22 =
    listWorkflow {
        for x in [1..5] do
        for a in ['a'..'e'] do 
        return (x,a)
        }

// Recall, however, that sequences may be lazy  and we are not (yet)     
let l222 =
            seq {
                for x in [1..5] do
                for a in ['a'..'e'] do 
                yield (x,a, x/0)   }

// ... if we replace seq with listWorkflow, we can see what happens

// other conveniences
// This works because of Zero 
let _ =  (listWorkflow{printf "no:\t %d\n" 42} : int list) 

// This thanks to Combine               

let xx = seq { yield 1
               yield 2}

let xy = listWorkflow { return 1
                        return 2}               

let xyz = listWorkflow {
    return 1
    return 2
    return 3
    return 4
    }                         

// If you want to  fully mimic seq and be lazy, you can.
// We list w/o further explanation a monad for lazy lists based
// on thunks, offered by the Lazy type in F#


type LList<'a> = LList of Lazy<'a list>
type LListBuilder() =
    member this.Bind(list, f) =
        List.collect f list

    member this.Return(x) =
        [x]

    member this.For(list, f) =
            this.Bind(list, f)  
    member this.ReturnFrom(LList f) =
            f.Force()
    member this.Zero() = []
    member this.Delay(f) = f
    member this.Run(f) =
        LList (lazy f())        
    
    member this.Combine(x,y) = x @ y

let llist =  LListBuilder()

let run (LList f) = f.Force()

let llexp  =
    llist{
        let! x = [1..10]
        return (x + x)
    }
let eval = run llexp

// and now, for the coup de resistance, the continuation monad ...