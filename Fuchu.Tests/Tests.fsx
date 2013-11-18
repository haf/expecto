#I @"..\lib"
#r "NUnit.Framework.dll"
#r @"..\packages\FSharpx.Core.1.8.41\lib\40\Fsharpx.Core.dll"
#r @"..\packages\FsCheck.0.9.1.0\lib\net40-Client\FsCheck.dll"
#load @"..\Fuchu\Fuchu.fs"
#load @"..\Fuchu\Assertions.fs"
#load @"..\Fuchu.FsCheck\FsCheck.fs"
#load @"Prelude.fs"
#load @"Tests.fs"

open System
open Fuchu

Tests.tests
|> Test.filter (fun s -> s.Contains "Exception handling")
|> run
