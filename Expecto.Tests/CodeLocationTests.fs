module Expecto.CodeLocationTests


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


type LetMeGetTheCurrentAssembly = private Case of unit 

[<Tests>]
let tests = testList "Code Location Tests" [

    let tryGetTestCode (test: Test) : TestCode option =
        let rec recurse (test: Test ) = 
            match test with 
            | TestCase (testCode, _) -> Some testCode
            | TestLabel (_, test, _) -> recurse test
            | _ -> None

        recurse test

    let getTestCode (test: Test) : TestCode = tryGetTestCode test |> Option.defaultWith (fun () -> failwith $"Count not find single TestCode for Test value: {test}")  
    
    let thisAssembly = System.Reflection.Assembly.GetAssembly(typeof<LetMeGetTheCurrentAssembly>)
    let pathToThisFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__)
    

    testList "it can locate the basic test functions" [
        // NOTE: Can't use testTheory because Test doesn't support equality, which is required by the stringify in testTheory
        let inline testLocation testName (sampleTest: Test) (expectedLocation: SourceLocation) =
            test testName {
                let testCode = getTestCode sampleTest 
                let actualLocation = Expecto.Impl.getLocation thisAssembly testCode

                Expect.equal actualLocation expectedLocation ""
            }
        
        testLocation "testCase" CodeLocationSamples.testCaseExample { sourcePath = pathToThisFile; lineNumber = 7 }
        testLocation "ptestCase" CodeLocationSamples.ptestCaseExample { sourcePath = pathToThisFile; lineNumber = 8 }
        testLocation "ftestCase" CodeLocationSamples.ftestCaseExample { sourcePath = pathToThisFile; lineNumber = 9 }

        test "testParam" {
            let testCodeList = CodeLocationSamples.testParamExamples |> List.map getTestCode
            let actualLocations = testCodeList |> List.map (Expecto.Impl.getLocation thisAssembly)
            let expectedLocations = [
                { sourcePath = pathToThisFile; lineNumber = 16 }
                { sourcePath = pathToThisFile; lineNumber = 17 }
            ]
            Expect.equal actualLocations expectedLocations ""
        }
    ]
    
]
