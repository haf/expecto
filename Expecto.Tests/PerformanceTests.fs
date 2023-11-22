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


open System.Runtime.InteropServices;
[<Tests>]
let performance =
  testSequenced <| testList "performance cryptography tests" [

    testCase "md5 equals sha256" (fun _ ->
      Expect.isFasterThan runMD5 runSHA256 "MD5 equals SHA256 should fail"
    ) |> assertTestFailsWithMsgContaining "same"
  ]

[<Tests>]
let findFastest =
  testSequenced <| testList "findFastest" [

    testCase "different values gives an error" (fun _ ->
      Performance.findFastest id 10 20 |> ignore
    ) |> assertTestFailsWithMsgStarting "Expected results to be the same."

    ptestCase "find fastest sleep" (fun _ ->
      let f i = Threading.Thread.Sleep(abs(i-65)*10)
      let result = Performance.findFastest f 0 100
      Expect.equal result 65 "find min"
    )

    ptestCase "find fastest hi" (fun _ ->
      let f i = Threading.Thread.Sleep(abs(i-110)*10)
      let result = Performance.findFastest f 0 100
      Expect.equal result 100 "find min"
    )

    ptestCase "find fastest lo" (fun _ ->
      let f i = Threading.Thread.Sleep(abs(i+10)*10)
      let result = Performance.findFastest f 0 100
      Expect.equal result 0 "find min"
    )
  ]
