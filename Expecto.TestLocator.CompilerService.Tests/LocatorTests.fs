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

[<Tests>]
let tests = testList "Code Location Tests" [
    
    let thisAssembly = System.Reflection.Assembly.GetAssembly(typeof<LetMeGetTheCurrentAssembly>)
    let pathToThisFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__)
    

    testList "it can locate the basic test functions" [
        // NOTE: Can't use testTheory because Test doesn't support equality, which is required by the stringify in testTheory
        let inline testLocation testName (sampleTest: Test) (expectedLocation: SourceLocation) =
            test testName {
                let flatTests = Test.toTestCodeList sampleTest
                let actualLocation = flatTests |> List.map (Sut.getTestLocation thisAssembly CodeLocationSamples.sampleFilePath)

                Expect.equal (actualLocation |> List.toArray) [|Some expectedLocation|] "" 
            }
        
        testLocation "testCase"  CodeLocationSamples.testCaseExample CodeLocationSamples.testCaseExampleLocation
        testLocation "ptestCase" CodeLocationSamples.ptestCaseExample CodeLocationSamples.ptestCaseExampleLocation
        testLocation "ftestCase" CodeLocationSamples.ftestCaseExample CodeLocationSamples.ftestCaseExampleLocation

        // test "testParam" {
        //     let testCodeList = CodeLocationSamples.testParamExamples |> List.map getTestCode
        //     let actualLocations = testCodeList |> List.map (Expecto.Impl.getLocation thisAssembly)
        //     let expectedLocations = [
        //         { sourcePath = pathToThisFile; lineNumber = 16 }
        //         { sourcePath = pathToThisFile; lineNumber = 17 }
        //     ]
        //     Expect.equal actualLocations expectedLocations ""
        // }
        
    ]
    
]