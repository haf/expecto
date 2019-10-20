module Expecto.Bug341Test
open Expecto

type TestDir =
  { Dir: string }
  interface System.IDisposable with
    member x.Dispose() = ()

let example () = { Dir = "" }

[<Tests>]
let tests =
  test "test" {
    use _ = example () // <- FS0001
    ()
  }
