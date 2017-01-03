module Main

open Expecto
open Expecto.Tests
open Expecto.Impl

[<EntryPoint>]
let main args =
  let localList = 
    testList "local" [
      ftestList "focused" [
        testCase "test" (fun () -> ())
      ]
    ]

  // check if we can fail on focused tests
  if runTests { defaultConfig with failOnFocusedTests = true } localList <> 1 then
    failwith "focused test check didn't fail"

  runTestsInAssembly defaultConfig args