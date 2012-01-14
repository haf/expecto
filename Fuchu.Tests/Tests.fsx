#r @"..\lib\NUnit.Framework.dll"
#load @"..\Fuchu\Fuchu.fs"
#load "Program.fs"

open Fuchu
open Fuchu.Tests

evalSilent tests
|> Seq.filter TestRunResult.isFailedOrException