namespace Fuchu

open System

module Seq = 
    let (|Empty|Cons|) l = 
        if Seq.isEmpty l
            then Empty
            else Cons(Seq.head l, Seq.skip 1 l)

    let (|One|_|) l = 
        let a = Seq.toList l
        if a.Length = 1
            then Some a.[0]
            else None

    let (|Two|_|) l = 
        let a = Seq.toList l
        if a.Length = 2
            then Some (a.[0], a.[1])
            else None