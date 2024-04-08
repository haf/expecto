#r "nuget:Expecto"
open Expecto

let simpleTest =
  testCase "A simple test" <| fun () ->
    Expect.equal 4 (2+2) "2+2 = 4"

runTestsWithCLIArgs [] [||] simpleTest