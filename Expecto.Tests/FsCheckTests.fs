module Expecto.FsCheckTests

open Expecto
open Expecto.Impl

let properties =
  testList "FsCheck" [
    testProperty "Addition is commutative" <|
      fun a b ->
        a + b = b + a

    testProperty "Product is distributive over addition" <|
      fun a b c ->
        a * (b + c) = a * a + a * c // wrong on purpose to test failures

    testProperty "ignored" <| fun _ -> skiptest "because reasons"
  ]

[<Tests>]
let runFsCheckTests =
  testCase "run" <| fun _ ->
    let results = evalSilent properties
    Expect.equal results.Length 3 "results length"
    Expect.equal results.[0].result TestResult.Passed "passed count"

    match results.[1].result with
    | TestResult.Failed _ ->
      ()
    | x ->
      failtestf "Expected Failed, actual %A" x

    match results.[2].result with
    | TestResult.Ignored e ->
      Expect.equal "because reasons" e "ignore description"
    | x ->
      failtestf "Expected Ignored, actual %A" x
