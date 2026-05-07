module LocatorTests 

open Expecto.TestLocator.CompilerService
open Expecto

type LetMeGetTheCurrentAssembly = private Case of unit 

module Sut =
    open System.Reflection
    open System.IO

    let mutable private locationCache : Map<string list, SourceLocation> option = None 
    let getTestLocation (assembly: Assembly) (sourceFilePath: string) (test: FlatTest) = 
        let locationMap = 
            match locationCache with
            | Some cache -> cache
            | None ->
                let map = FCSTestLocator.getTestNameToLocationMapForFile sourceFilePath |> Async.RunSynchronously 
                locationCache <- Some map
                map 
                
        locationMap |> Map.tryFind test.name

let thisAssembly = System.Reflection.Assembly.GetAssembly(typeof<LetMeGetTheCurrentAssembly>)

let testLocation (sampleTest: CodeLocationSamples.LocationTestExample) =
    test sampleTest.Name {
        let flatTests = Test.toTestCodeList sampleTest.Test
        let actualLocations = 
            flatTests 
            |> List.choose (Sut.getTestLocation thisAssembly CodeLocationSamples.samplesFilePath)
            |> Array.ofList

        Expect.equal actualLocations sampleTest.ExpectedLocations "" 
    }
[<Tests>]
let tests = testList "Code Location Tests" [
    testList "testCase variants" (CodeLocationSamples.testCaseExamples |> List.map testLocation)
    testList "test builder variants" (CodeLocationSamples.testBuilderExamples |> List.map testLocation)
]