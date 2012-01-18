#r @"..\lib\NUnit.Framework.dll"
#load @"..\Fuchu\Fuchu.fs"
#load "Program.fs"

open Fuchu
open Fuchu.Tests

tests
|> Test.filter (fun n -> n.Contains "Exception")
|> evalSilent

//evalSilent tests
//|> Seq.filter TestRunResult.isFailedOrException