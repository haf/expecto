module Expecto.Focused.Tests

open Expecto


(*
  This test is based on the fact that the unfocusedAttributeDiscoveredTest will be discovered
  by using Tests attribute and normally should fail, unless the focusedAttributeDiscoveredTest
  is also discovered and the focusing orchestration is taking place.
*)


let failing = fun _ -> Expect.isTrue(false) "expect failing"
let working = ignore

[<FTests>]
let focusedAttributeDiscoveredTest = testCase "FTestAttribute is working as intended" working

[<Tests>]
let unfocusedAttributeDiscoveredTest =  testCase "Should fail if FTestAttribute is not working" failing


[<EntryPoint>]
let main args =
  args
  |> Array.filter(fun a -> a.ToLower().Trim() <> "--fail-on-focused-tests")
  |> runTestsInThisAssembly { defaultConfig with failOnFocusedTests = false }