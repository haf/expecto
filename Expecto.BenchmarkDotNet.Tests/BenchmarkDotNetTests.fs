open Expecto

type ISerialiser =
  abstract member Serialise<'a> : 'a -> unit

type MySlowSerialiser() =
  interface ISerialiser with
    member __.Serialise _ =
      System.Threading.Thread.Sleep(30)

type FastSerialiser() =
  interface ISerialiser with
    member __.Serialise _ =
      System.Threading.Thread.Sleep(10)

type FastSerialiserAlt() =
  interface ISerialiser with
    member __.Serialise _ =
     System.Threading.Thread.Sleep(20)

type Serialisers() =
  let fast, fastAlt, slow =
    FastSerialiser() :> ISerialiser,
    FastSerialiserAlt() :> ISerialiser,
    MySlowSerialiser() :> ISerialiser

  [<Benchmark>]
  member __.FastSerialiserAlt() = fastAlt.Serialise "Hello world"

  [<Benchmark>]
  member __.SlowSerialiser() = slow.Serialise "Hello world"

  [<Benchmark(Baseline = true)>]
  member __.FastSerialiser() = fast.Serialise "Hello world"

[<Tests>]
let tests =
  testList "performance tests" [
    test "three serialisers" {
      benchmark<Serialisers> benchmarkConfig (fun _ -> null) |> ignore
    }
  ]

[<EntryPoint>]
let main args =
    runTestsInAssembly defaultConfig args
