module Expecto.FsCheckTests

open Expecto
open Expecto.Impl

let properties =
  testList "FsCheck" [
    testProperty "Addition is commutative" <|
      fun a b ->
        a + b = b + a

    testProperty "Deliberately failing test" <|
      fun a b c ->
        // wrong on purpose to test failures
        a * (b + c) = a * a + a * c

    testProperty "ignored" <| fun _ ->
      skiptest "Because I feel like it."
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
      Expect.equal "Because I feel like it." e "It should fail with the right message."
    | x ->
      failtestf "Expected Ignored, actual %A" x
