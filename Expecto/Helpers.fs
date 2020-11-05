[<AutoOpen>]
module internal Expecto.Helpers

open System
open System.Globalization
open System.Reflection

let expectoVersion =
  Assembly.GetExecutingAssembly()
    .GetCustomAttribute<AssemblyFileVersionAttribute>()
    .Version

let inline dispose (d:IDisposable) = d.Dispose()
let inline addFst a b = a,b
let inline addSnd b a = a,b
let inline fst3 (a,_,_) = a
let inline commaString (i:int) = i.ToString("#,##0")
let inline tryParse (s: string) =
  let mutable r = Unchecked.defaultof<_>
  if (^a : (static member TryParse: string * ^a byref -> bool) (s, &r))
  then Some r else None
let inline tryParseNumber (s: string) =
  let mutable r = Unchecked.defaultof<_>
  if (^a : (static member TryParse: string * NumberStyles * IFormatProvider * ^a byref -> bool) (s, NumberStyles.Any, CultureInfo.InvariantCulture, &r))
  then Some r else None

module Seq =
  let cons x xs = seq { yield x; yield! xs }

module List =
  let inline singleton x = [x]

module Array =
  let shuffleInPlace (a:_ array) =
    let rand = Random()
    for i = Array.length a - 1 downto 1 do
      let j = rand.Next(i+1)
      if i<>j then
          let temp = a.[j]
          a.[j] <- a.[i]
          a.[i] <- temp

module Option =
  let orFun fn =
    function | Some a -> a | None -> fn()
  let orDefault def =
    function | Some a -> a | None -> def

module Result =
  let traverse f list =
    List.fold (fun s i ->
        match s,f i with
        | Ok l, Ok h -> Ok (h::l)
        | Error l, Ok _ -> Error l
        | Ok _, Error e -> Error [e]
        | Error l, Error h -> Error (h::l)
    ) (Ok []) list
  let sequence list = traverse id list

type Type with
  static member TryGetType t =
    try
      Type.GetType(t, true) |> Some
    with _ ->
      None

type ResizeMap<'k,'v> = Collections.Generic.Dictionary<'k,'v>

let matchFocusAttributes = function
  | "Expecto.FTestsAttribute" -> Some (1, Focused)
  | "Expecto.TestsAttribute" -> Some (2, Normal)
  | "Expecto.PTestsAttribute" -> Some (3, Pending)
  | _ -> None

let inline tryParseFocusState (input: string) =
  let inline (=~) (input : string) (value: string) =
    input.Equals(value, StringComparison.OrdinalIgnoreCase)
  if   input =~ "focused" then Some Focused
  elif input =~ "pending" then Some Pending
  elif input =~ "normal" then Some Normal
  else None

let allTestAttributes =
  Set [
    typeof<FTestsAttribute>.FullName
    typeof<TestsAttribute>.FullName
    typeof<PTestsAttribute>.FullName
  ]

type MemberInfo with
  member m.HasAttributePred (pred: Type -> bool) =
    m.GetCustomAttributes true
    |> Seq.filter (fun a -> pred(a.GetType()))
    |> Seq.length |> (<) 0

  member m.HasAttributeType (attr: Type) =
    m.HasAttributePred ((=) attr)

  member m.HasAttribute (attr: string) =
    m.HasAttributePred (fun (t: Type) -> t.FullName = attr)

  member m.GetAttributes (attr: string) : Attribute seq =
    m.GetCustomAttributes true
    |> Seq.filter (fun a -> a.GetType().FullName = attr)
    |> Seq.cast

  member m.MatchTestsAttributes () =
    m.GetCustomAttributes true
    |> Seq.map (fun t -> t.GetType().FullName)
    |> Set.ofSeq
    |> Set.intersect allTestAttributes
    |> Set.toList
    |> List.choose matchFocusAttributes
    |> List.sortBy fst
    |> List.map snd
    |> List.tryHead
