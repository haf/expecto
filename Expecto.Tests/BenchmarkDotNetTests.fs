module Expecto.BenchmarkDotNetTests

open System
open System.Security.Cryptography
open Expecto
open BenchmarkDotNet

type Md5VsSha256() =
  let data : byte[] = Array.zeroCreate 10000
  do Random(42).NextBytes data

  let md5, sha256 = MD5.Create(), SHA256.Create()

  [<Benchmark>]
  member __.Sha256() = sha256.ComputeHash data
  [<Benchmark>]
  member __.Md5() = md5.ComputeHash data

#if DEBUG
open BenchmarkDotNet.Jobs

// A benchmark is not so useful in Debug builds, so just make it fast
let theConfig =
  { benchmarkConfig with
      jobs = [ Job.Default
                .WithLaunchCount(Count 1)
                .WithIterationTime(Count 50)
                .WithTargetCount(Count 1)
                .WithWarmupCount(Count 2) ] }
#else
let theConfig = benchmarkConfig
#endif

[<Tests>]
let benchmarks =
  testSequenced <| testList "some different benchmarks" [
    benchmark<Md5VsSha256> "md5 versus sha256" theConfig ignore
  ]



[<Tests>]
let performance =
  testSequenced <| testList "performance cryptography tests" [

    testCase "md5 equals sha256" (fun _ ->
      let a = Md5VsSha256()
      Expect.isFasterThan a.Md5 a.Sha256 "MD5 equals SHA256 should fail"
    ) |> assertTestFailsWithMsgContaining "same"

    testCase "sha256 versus md5" (fun _ ->
      let a = Md5VsSha256()
      Expect.isFasterThan (a.Sha256 >> ignore) (a.Md5 >> ignore) "SHA256 is faster than MD5 should fail"
    ) |> assertTestFailsWithMsgContaining "slower"

    testCase "md5 versus sha256" <| fun _ ->
      let a = Md5VsSha256()
      Expect.isFasterThan (a.Md5 >> ignore) (a.Sha256 >> ignore) "MD5 is faster than SHA256"

    ]