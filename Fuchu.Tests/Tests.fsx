#I @"..\lib"
#r "NUnit.Framework.dll"
#r "FSharpx.Core.dll"
#load @"..\Fuchu\Fuchu.fs"
#load @"Prelude.fs"
#load @"Tests.fs"

open System
open Fuchu
open Fuchu.Tests

tests
|> Test.filter (fun n -> n.Contains "Reflection")
|> run
