module Expecto.CodeLocationTests


module CodeLocationSamples = 


    let testCaseExample = testCase "testCase" <| (fun () -> ())
    let builderExample = test "Locate a builder" { ignore ()}

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
    

    test "locate a testCase" {
        let testCode = getTestCode CodeLocationSamples.testCaseExample 

        let expectedLocation = { sourcePath = pathToThisFile; lineNumber = 7}
        let actualLocation = Expecto.Impl.getLocation thisAssembly testCode

        Expect.equal actualLocation expectedLocation ""
    }
]
