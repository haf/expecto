#I @"..\lib"
#r "NUnit.Framework.dll"
#r "Gallio.dll"
#r "MbUnit.dll"
#r "FSharpx.Core.dll"
#load @"..\Fuchu\Fuchu.fs"
#load @"..\Fuchu\xUnitHelpers.fs"
#load @"..\Fuchu\Fuchu.MbUnit.fs"
#load @"Prelude.fs"
#load @"MbUnitTestTypes.fs"
#load @"MbUnitTests.fs"

open System
open Fuchu
open Fuchu.MbUnitTests

tests
//|> Test.filter (fun n -> n.Contains "parse args")
|> run
