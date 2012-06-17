#I @"..\lib"
#r "NUnit.Framework.dll"
#r @"..\packages\FsCheck.0.7.1\lib\net35\FsCheck.dll"
#r "FSharpx.Core.dll"
#load @"..\Fuchu\Fuchu.fs"
#load @"..\Fuchu.FsCheck\FsCheck.fs"
#load @"FsCheckTests.fs"

open System
open Fuchu

FsCheckTests.runFsCheckTests
|> run
