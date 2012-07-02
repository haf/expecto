#I @"..\lib"
#r "NUnit.Framework.dll"
#r "FSharpx.Core.dll"
#r @"..\packages\FsCheck.0.8.1.0\lib\net40-Client\FsCheck.dll"
#load @"..\Fuchu\Fuchu.fs"
#load @"..\Fuchu.FsCheck\FsCheck.fs"
#load @"Prelude.fs"
#load @"Tests.fs"

open System
open Fuchu

Tests.tests
|> Test.filter (fun s -> s.Contains "Exception handling")
|> run
