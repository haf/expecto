module Expecto.Flip.Sample

module Utils =
  let repeatN n f a =
    let mutable v = f a
    for n in 2 .. n do
      v <- f a
    v
  let repeat10 f a = repeatN 10 f a
  let repeat100 f a = repeatN 100 f a
  let repeat1000 f a = repeatN 1000 f a
  let repeat10000 f a = repeatN 10000 f a

open Utils
open Expecto
open Expecto.Flip

[<Tests>]
let tests =
  testList "flipped samples" [
    testCase "universe exists (╭ರᴥ•́)" <| fun _ ->
      let subject = true
      subject |> Expect.isTrue "I compute, therefore I am."

    testCase "when true is not (should fail)" <| fun _ ->
      let subject = false
      subject |> Expect.isTrue "I should fail because the subject is false"

    testCase "I'm skipped (should skip)" <| fun _ ->
      Tests.skiptest "Yup, waiting for a sunny day..."

    testCase "I'm always fail (should fail)" <| fun _ ->
      Tests.failtest "This was expected..."

    testCase "contains things" <| fun _ ->
      [| 2; 3; 4 |] 
      |> Expect.containsAll "This is the case; {2,3,4} contains {2,4}" [| 2; 4 |] 

    testCase "contains things (should fail)" <| fun _ ->
      [| 2; 3; 4 |] 
      |> Expect.containsAll "Expecting we have one (1) in there" [| 2; 4; 1 |]

    testCase "Sometimes I want to ༼ノಠل͟ಠ༽ノ ︵ ┻━┻" <| fun _ ->
      "abcdëf" |> Expect.equal "These should equal" "abcdef"

    test "I am (should fail)" {
       false |> Expect.equal "╰〳 ಠ 益 ಠೃ 〵╯" true
    }

    testCase "is ascending" <| fun _ ->
      [0..100] |> Expect.isAscending "expected sequence to be ascending"

    testCase "You know exns" <| fun _ ->
      failwith "unhandled exception from test code"

    ptestCase "I'm pending" <| fun _ -> ()

    // uncomment me:
    //ftestCase "I'm focused, I will cause all other tests to be skipped" <| fun () -> ()

    testSequenced (
      testCase "fn1 faster than fn2" <| fun _ ->
        (fun () -> repeat10000 log 76.0) 
        |> Expect.isFasterThan "half is faster" (fun () -> repeat10000 log 76.0 |> ignore; repeat10000 log 76.0)
    )

    testCaseAsync "simple, not doing very much" <| async {
      1 |> Expect.equal "1=1" 1
      do! async.Zero ()
    }

    test "Should be close" {
      let actual, expected = 41.5621, 41.5620
      actual |> Expect.floatClose "Should be close within 5 sig figs (approx)" Accuracy.medium expected
    }

    test "Should not be close enough (should fail)" {
      let actual, expected = 41.562, 41.563
      actual |> Expect.floatClose "Should be close within 5 sig figs (approx)" Accuracy.medium expected
    }

    testCase "1 is > to 0" <| fun () ->
      (1, 0) |> Expect.isGreaterThan "1 > 0"
    
    testCase "0 is > to 1 (should fail)" <| fun () ->
      (0, 1) |> Expect.isGreaterThan "1 > 0"
    
    testProperty "addition is commutative" <| fun a b ->
      a + b = b + a

    testPropertyWithConfig FsCheckConfig.defaultConfig "Product is distributive over addition" <|
      fun a b c ->
        a * (b + c) = a * b + a * c
  ]

