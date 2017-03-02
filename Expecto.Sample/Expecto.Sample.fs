module Expecto.Sample

module Utils =
  let inline repeat10 f a =
    let mutable v = f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v <- f a
    v
  let inline repeat100 f a = repeat10 (repeat10 f) a
  let inline repeat1000 f a = repeat10 (repeat100 f) a
  let inline repeat10000 f a = repeat10 (repeat1000 f) a

open Utils
open Expecto


[<Tests>]
let tests =
  testList "samples" [
    testCase "universe exists (╭ರᴥ•́)" <| fun _ ->
      let subject = true
      Expect.isTrue subject "I compute, therefore I am."

    testCase "when true is not (should fail)" <| fun _ ->
      let subject = false
      Expect.isTrue subject "I should fail because the subject is false"

    testCase "I'm skipped (should skip)" <| fun _ ->
      Tests.skiptest "Yup, waiting for a sunny day..."

    testCase "I'm always fail (should fail)" <| fun _ ->
      Tests.failtest "This was expected..."

    testCase "contains things" <| fun _ ->
      Expect.containsAll [| 2; 3; 4 |] [| 2; 4 |]
                         "This is the case; {2,3,4} contains {2,4}"

    testCase "contains things (should fail)" <| fun _ ->
      Expect.containsAll [| 2; 3; 4 |] [| 2; 4; 1 |]
                         "Expecting we have one (1) in there"

    testCase "Sometimes I want to ༼ノಠل͟ಠ༽ノ ︵ ┻━┻" <| fun _ ->
      Expect.equal "abcdëf" "abcdef" "These should equal"

    test "I am (should fail)" {
      "╰〳 ಠ 益 ಠೃ 〵╯" |> Expect.equal true false
    }

    testCase "You know exns" <| fun _ ->
      failwith "unhandled exception from test code"

    ptestCase "I'm pending" <| fun _ -> ()

    // uncomment me:
    //ftestCase "I'm focused, I will cause all other tests to be skipped" <| fun () -> ()

    testSequenced (
      testCase "fn1 faster than fn2" <| fun _ ->
        Expect.isFasterThan (fun () -> repeat10000 log 76.0)
                            (fun () -> repeat10000 log 76.0 |> ignore; repeat10000 log 76.0)
                            "half is faster"
    )

    testCaseAsync "simple, not doing very much" <| async {
      Expect.equal 1 1 "1=1"
      do! async.Zero ()
    }

    test "Should be close" {
      let actual, expected = 41.5621, 41.5620
      Expect.floatClose Accuracy.medium actual expected "Should be close within 5 sig figs (approx)"
    }

    test "Should not be close enough (should fail)" {
      let actual, expected = 41.562, 41.563
      Expect.floatClose Accuracy.medium actual expected "Should be close within 5 sig figs (approx)"
    }

    testProperty "addition is commutative" <| fun a b ->
      a + b = b + a

    testPropertyWithConfig FsCheckConfig.defaultConfig "Product is distributive over addition" <|
      fun a b c ->
        a * (b + c) = a * b + a * c
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInThisAssembly defaultConfig argv