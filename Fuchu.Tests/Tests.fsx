#r @"..\lib\NUnit.Framework.dll"
#r @"..\lib\FSharpx.Core.dll"
#load @"..\Fuchu\Fuchu.fs"
#load "Program.fs"

open Fuchu
open Fuchu.Tests

tests
|> Test.filter (fun n -> n.Contains "NUnit")
|> evalSilent

//evalSilent tests
//|> Seq.filter TestRunResult.isFailedOrException