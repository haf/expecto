module LocatorTests 

open Expecto.TestLocator.CompilerService
open Expecto

module CodeLocationSamples = 

    let testCaseExample = testCase "testCase" <| (fun () -> ())
    let ptestCaseExample = ptestCase "ptestCaseExample" <| (fun () -> ())
    let ftestCaseExample = ftestCase "ftestCaseExample" <| (fun () -> ())

    let testBuilderExample = test "Locate a builder" { ignore () }
    let ptestBuilderExample = ptest "Locate a builder" { ignore () }
    let ftestBuilderExample = ftest "Locate a builder" { ignore () }

    let testParamExamples = List.ofSeq (testParam 5 [
            "t1", (fun _ () -> ())
            "t2", (fun _ () -> ())
        ])

    let testCaseAsyncExample = testCaseAsync "testCaseExample" <| async { ignore ()}

    let testTheoryExample = testTheory "testTheory example" [1;2] (fun _ -> ())

    let testListExample = testList "testList example" []


type LetMeGetTheCurrentAssembly = private Case of unit 

module Sut =
    open System.Reflection
    open System.IO

    let mutable private locationCache : Map<string list, SourceLocation> option = None 
    let getTestLocation (assembly: Assembly) (test: FlatTest) = 
        let projectFilePath = Path.Combine(__SOURCE_DIRECTORY__, "Expecto.TestLocator.CompilerService.fsproj")
        // let projectFilePath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        
        let locationMap = 
            match locationCache with
            | Some cache -> cache
            | None ->
                printfn $"generating map for project: {projectFilePath}"
                let map = FCSTestLocator.getTestNameToLocationMap projectFilePath |> Async.RunSynchronously 
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
                let actualLocation = flatTests |> List.map (Sut.getTestLocation thisAssembly)

                Expect.equal (actualLocation |> List.toArray) [|Some expectedLocation|] "" 
            }
        
        testLocation "testCase" CodeLocationSamples.testCaseExample { sourcePath = pathToThisFile; lineNumber = 8 }
        testLocation "ptestCase" CodeLocationSamples.ptestCaseExample { sourcePath = pathToThisFile; lineNumber = 9 }
        testLocation "ftestCase" CodeLocationSamples.ftestCaseExample { sourcePath = pathToThisFile; lineNumber = 10 }

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