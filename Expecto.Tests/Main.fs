module Main

open Expecto

[<EntryPoint>]
let main args =
  // check if we can fail on focused tests
  if runTestsInAssembly { defaultConfig with fail_on_focused_tests = true } args <> 1 then
    failwith "focused test check didn't fail"

  runTestsInAssembly defaultConfig args