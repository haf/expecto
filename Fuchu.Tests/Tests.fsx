// #r @"..\lib\NUnit.Framework.dll" 
//#r @"..\lib\FSharpx.Core.dll"
#load @"..\Fuchu\Fuchu.fs"
//#load "Prelude.fs"
//#load "NUnitTestTypes.fs"
//#load "Program.fs"

open System
open Fuchu

type Dummy = class end

module Pepe = 
    let toto = []
    let totof() = []

let pepeType = 
    typeof<Dummy>.Assembly.GetExportedTypes() 
    |> Seq.filter (fun t -> t.Name = "Pepe")
    |> Seq.head


//open Fuchu.Tests
//
//tests
//|> Test.filter (fun n -> not <| n.Contains "NUnit")
//|> evalSilent
//|> Seq.filter TestRunResult.isFailedOrException

//evalSilent tests
//|> Seq.filter TestRunResult.isFailedOrException