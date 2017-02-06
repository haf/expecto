namespace Expecto
#nowarn "44"

open System

module Seq =
  let (|Empty|Cons|) l =
    if Seq.isEmpty l then
      Empty
    else
      Cons(Seq.head l, Seq.skip 1 l)

  let (|One|_|) l =
    match Seq.toList l with
    | [x] -> Some x
    | _ -> None

  let (|Two|_|) l =
    match Seq.toList l with
    | [x;y] -> Some(x,y)
    | _ -> None

[<AutoOpen>]
module TestHelpers =
  open Expecto
  open Expecto.Impl

  let rec private makeTest originalTest asyncTestFn =
    match originalTest with
    | TestCase (_,state)
    | TestList (_,state) ->
      TestCase(Async asyncTestFn, state)
    | TestLabel (label,_,state) ->
      TestLabel (label, TestCase(Async asyncTestFn, state), state)
    | Test.Sequenced (sequenced,test) ->
      Test.Sequenced (sequenced,makeTest test asyncTestFn)


  let assertTestFails test =
    async {
      let! result = Impl.evalTestsSilent test
      match result with
      | [(_,{ result = TestResult.Ignored _ })] -> ()
      | [(_,{ result = TestResult.Failed _ })] -> ()
      | [x] -> failtestf "Should have failed, but was %A" x
      | _ -> failtestf "Should have one test to assert"
    } |> makeTest test

  let assertTestFailsWithMsgStarting (msg : string) test =
    async {
      let! result = Impl.evalTestsSilent test
      match result with
      | [(_,{ result = TestResult.Ignored _ })] -> ()
      | [(_,{ result = TestResult.Failed x })] ->
        let removeCR = x.Replace("\r","").Trim('\n')
        Expect.stringStarts removeCR msg "Test failure strings should equal"
      | [x] -> failtestf "Should have failed, but was %A" x
      | _ -> failtestf "Should have one test to assert"
    } |> makeTest test

  let assertTestFailsWithMsgContaining (msg : string) test =
    async {
      let! result = Impl.evalTestsSilent test
      match result with
      | [(_,{ result = TestResult.Ignored _ })] -> ()
      | [(_,{ result = TestResult.Failed x })] when x.Contains msg -> ()
      | [(_,{ result = TestResult.Failed x })] ->
        failtestf "Should have failed with message containing: \"%s\" but failed with \"%s\"" msg x
      | [x] -> failtestf "Should have failed, but was %A" x
      | _ -> failtestf "Should have one test to assert"
    } |> makeTest test

  module Generator =
    open FsCheck
    type private Marker = class end
    let arbs = typeof<Marker>.DeclaringType
    let exn = Arb.convert exn string Arb.from
    let testCode = Sync ignore |> Gen.constant |> Arb.fromGen
    let timespan =
      Arb.convert
        (float >> TimeSpan.FromMilliseconds)
        (fun (t:TimeSpan) -> t.TotalMilliseconds |> uint16)
        Arb.from

  let inline repeat10 f a =
    let mutable v = f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v
  let inline repeat100 f a = repeat10 (repeat10 f) a
  let inline repeat1000 f a = repeat10 (repeat100 f) a
  let inline repeat10000 f a = repeat10 (repeat1000 f) a
  let inline repeat100000 f a = repeat10 (repeat10000 f) a
  let inline repeat1000000 f a = repeat10 (repeat100000 f) a