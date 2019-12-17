module Expecto.Bug367Test
open Expecto
open System

let createWork ()=
  let printer = defaultConfig.printer
  [ for i in 1 .. 10000 ->
    async {
      let testName = (sprintf "print test %d" i)
      let! child = printer.beforeEach testName |> Async.StartChild
      do! child
      do! printer.passed testName (TimeSpan.FromSeconds 0.01)
      return 1
    } ]
[<Tests>]
let tests =
  testCaseAsync "deadlockonlogging_seq" <| async {
    let printer = defaultConfig.printer
    let! tok = Async.CancellationToken
    let! res =
      createWork()
      |> Async.foldSequentiallyWithCancel tok (fun j i -> j + i) 0
    Expect.equal res 10000 "should have folded all items"
  }

  testCaseAsync "deadlockonlogging_par" <| async {
    let! tok = Async.CancellationToken
    let! res =
      createWork()
      |> Async.foldParallelWithCancel 4 tok (fun j i -> j + i) 0
    Expect.equal res 10000 "should have folded all items"
  }
