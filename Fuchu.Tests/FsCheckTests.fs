namespace Fuchu

module FsCheckTests = 
    open Fuchu
    open Fuchu.Impl
    open Fuchu.FsCheck

    let properties = 
        testList "FsCheck" [
            testProperty "Addition is commutative" <|
                fun a b -> 
                    a + b = b + a
            testProperty "Product is distributive over addition" <|
                fun a b c -> 
                    a * (b + c) = a * a + a * c
        ]

    open NUnit.Framework
        
    [<Tests>]
    let runFsCheckTests = 
        testCase "run" <|
            fun _ -> 
                let results = evalSilent properties
                Assert.AreEqual(2, results.Length)
                Assert.AreEqual(TestResult.Passed, results.[0].Result)
                match results.[1].Result with
                | TestResult.Failed _ -> ()
                | x -> raise <| AssertException (sprintf "Expected Failed, actual %A" x)
