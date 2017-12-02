module Expecto.PerformanceTests

open System
open System.Security.Cryptography
open Expecto

let md5 = MD5.Create()
let sha256 = SHA256.Create()

let data =
  let d = Array.zeroCreate 10000
  Random(42).NextBytes d
  d

let runMD5() = md5.ComputeHash data
let runSHA256() = sha256.ComputeHash data


[<Tests>]
let performance =
  testSequenced <| testList "performance cryptography tests" [

    testCase "md5 equals sha256" (fun _ ->
      Expect.isFasterThan runMD5 runSHA256 "MD5 equals SHA256 should fail"
    ) |> assertTestFailsWithMsgContaining "same"

    testCase "sha256 versus md5" (fun _ ->
      Expect.isFasterThan (runSHA256 >> ignore) (runMD5 >> ignore) "SHA256 is faster than MD5 should fail"
    ) |> assertTestFailsWithMsgContaining "slower"

    testCase "md5 versus sha256" <| fun _ ->
      Expect.isFasterThan (runMD5 >> ignore) (runSHA256 >> ignore) "MD5 is faster than SHA256"
    ]

[<Tests>]
let findFastest =
  testSequenced <| ftestList "findFastest" [

    testCase "different values gives an error" (fun _ ->
      Performance.findFastest id 10 20 |> ignore
    ) |> assertTestFailsWithMsgStarting "Expected results to be the same."

    testCase "find fastest sleep" (fun _ ->
      let f i = Threading.Thread.Sleep(abs(i-65)*10)
      let result = Performance.findFastest f 0 100
      Expect.equal result 65 "find min"
    )

    testCase "find fastest hi" (fun _ ->
      let f i = Threading.Thread.Sleep(abs(i-110)*10)
      let result = Performance.findFastest f 0 100
      Expect.equal result 100 "find min"
    )

    testCase "find fastest lo" (fun _ ->
      let f i = Threading.Thread.Sleep(abs(i+10)*10)
      let result = Performance.findFastest f 0 100
      Expect.equal result 0 "find min"
    )
  ]
