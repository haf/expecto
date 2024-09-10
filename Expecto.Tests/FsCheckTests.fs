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

    ptestProperty "ignored2" <| ignore

  ]

[<Tests>]
let runFsCheckTests =
  testCaseAsync "run" <| async {
    let! results = Impl.evalTestsSilent properties
    Expect.equal results.Length 4 "results length"

    let getResult name =
      results
      |> Seq.filter (fun (r,_) -> r.name=name)
      |> Seq.head
      |> snd

    Expect.equal (getResult ["FsCheck";"Addition is commutative"]).result
                 TestResult.Passed "passed count"

    match (getResult ["FsCheck";"Deliberately failing test"]).result with
    | TestResult.Failed _ ->
      ()
    | x ->
      failtestf "Expected Failed, actual %A" x

    match (getResult ["FsCheck";"ignored"]).result with
    | TestResult.Ignored e ->
      Expect.equal "Because I feel like it." e "It should fail with the right message."
    | x ->
      failtestf "Expected Ignored, actual %A" x

    match (getResult ["FsCheck";"ignored2"]).result with
    | TestResult.Ignored _ ->
      ()
    | x ->
      failtestf "Expected Ignored, actual %A" x

  }

let focused =
  testList "FsCheck focused" [
    testCase "ignore me" <| ignore

    etestProperty (1UL,2UL,0) "Deliberately failing test" <|
      fun a b c ->
        // wrong on purpose to test failures
        a * (b + c) = a * a + a * c
  ]

[<Tests>]
let runFsCheckFocusedTests =
  testCaseAsync "focused" <| async {
    let! results = Impl.evalTestsSilent focused
    Expect.equal results.Length 2 "results length"

    let getResult name =
      results
      |> Seq.filter (fun (r,_) -> r.name=name)
      |> Seq.head
      |> snd

    match (getResult ["FsCheck focused";"ignore me"]).result with
    | TestResult.Ignored _ ->
      ()
    | x ->
      failtestf "Expected Ignored, actual %A" x

    match (getResult ["FsCheck focused";"Deliberately failing test"]).result with
    | TestResult.Failed actual ->
      let expected = "
Failed after 5 tests. Parameters:
	-3 -1 0
Shrunk 4 times to:
	1 0 0
Result:
	False
Focus on error:
	etestProperty (1UL, 2UL, 0) \"Deliberately failing test\""
      Expect.equal actual expected "It should fail with the right message"
    | x ->
      failtestf "Expected Failed, actual was: %A" x
  }

let config =
  testList "FsCheck config" [
    testCase "ignore me" ignore

    etestPropertyWithConfig (1UL,2UL,0) FsCheckConfig.defaultConfig
      "Deliberately failing test" <|
      fun a b c ->
        // wrong on purpose to test failures
        a * (b + c) = a * a + a * c
  ]

[<Tests>]
let runFsCheckConfigTests =
  testCaseAsync "config" <| async {
    let! results = Impl.evalTestsSilent config
    Expect.equal results.Length 2 "results length"

    let getResult name =
      results
      |> Seq.filter (fun (r,_) -> r.name=name)
      |> Seq.head
      |> snd

    match (getResult ["FsCheck config";"ignore me"]).result with
    | TestResult.Ignored _ ->
      ()
    | x ->
      failtestf "Expected Ignored, actual %A" x

    match (getResult ["FsCheck config";"Deliberately failing test"]).result with
    | TestResult.Failed actual ->
      let expected = "
Failed after 5 tests. Parameters:
	-3 -1 0
Shrunk 4 times to:
	1 0 0
Result:
	False
Focus on error:
	etestPropertyWithConfig (1UL, 2UL, 0) \"Deliberately failing test\""
      Expect.equal actual expected "It should fail with the right message."

    | x ->
      failtestf "Expected Failed, actual %A" x
  }