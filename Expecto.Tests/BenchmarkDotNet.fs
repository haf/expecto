module Expecto.BenchmarkDotNetTests

open System
open System.Security.Cryptography
open Expecto
open BenchmarkDotNet

type Md5VsSha256() =
  [<Literal>]
  let N = 10000
  let data : byte[] = Array.zeroCreate N
  let sha256 = SHA256.Create()
  let md5 = MD5.Create()
  do Random(42).NextBytes data

  [<Benchmark>]
  member x.Sha256() = sha256.ComputeHash data
  [<Benchmark>]
  member x.Md5() = md5.ComputeHash(data)

[<Tests>]
let benchmarks =
  testList "some different benchmarks" [
    benchmark<Md5VsSha256> "md5 versus sha256" benchmarkConfig ignore
  ]