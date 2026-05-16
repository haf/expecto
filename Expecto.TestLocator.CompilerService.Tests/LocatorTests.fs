module LocatorTests 

open Expecto.TestLocator.CompilerService
open Expecto

type LetMeGetTheCurrentAssembly = private Case of unit 

let thisAssembly = System.Reflection.Assembly.GetAssembly(typeof<LetMeGetTheCurrentAssembly>)

let testLocation (sampleTest: CodeLocationSamples.LocationTestExample) =
    test sampleTest.Name {
        let flatTests = Test.toTestCodeList sampleTest.Test
        let actualLocations = 
            flatTests 
            |> List.choose (FCSTestLocator.testLocator thisAssembly)
            |> Array.ofList

        Expect.equal actualLocations sampleTest.ExpectedLocations "" 
    }
[<Tests>]
let tests = testList "Code Location Tests" [
    testList "testCase variants" (CodeLocationSamples.testCaseExamples |> List.map testLocation)
    testList "test builder variants" (CodeLocationSamples.testBuilderExamples |> List.map testLocation)
]