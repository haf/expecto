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
  member x.Sha256() = sha256.ComputeHash data
  [<Benchmark>]
  member x.Md5() = md5.ComputeHash(data)

[<Tests>]
let benchmarks =
  testList "some different benchmarks" [
    benchmark<Md5VsSha256> "md5 versus sha256" benchmarkConfig ignore
  ]