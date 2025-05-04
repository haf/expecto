#r "nuget:Expecto"
open Expecto
open Expecto.Flip
let simpleTest =                    
    (2+2)|>Expect.equal "2+2 = 4" 4

test "" {simpleTest} |> runTestsWithCLIArgs [] [||]