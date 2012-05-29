namespace Fuchu

open System

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArraySegment =

    let head (a: _ ArraySegment) =
        if a.Count = 0
            then raise (ArgumentException("The input array segment was empty"))
            else a.Array.[a.Offset]

    let tail (a: _ ArraySegment) =
        if a.Count = 0 
            then raise (ArgumentException("The input array segment was empty"))
            else ArraySegment(a.Array, a.Offset+1, a.Count-1)
            
    let (|Empty|Cons|) (a: _ ArraySegment) =
        if a.Count = 0 
            then Empty
            else Cons(head a, tail a)

module Option =
    let fromArraySegment =
        function
        | ArraySegment.Empty -> None
        | ArraySegment.Cons (x,xs) -> Some x

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