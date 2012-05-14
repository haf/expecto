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