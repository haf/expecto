#I @"..\lib"
#r "NUnit.Framework.dll"
#load @"..\Fuchu\Fuchu.fs"
#load @"..\Fuchu\xUnitHelpers.fs"
#load @"..\Fuchu\Fuchu.MbUnit.fs"
#r "Gallio.dll"
#r "MbUnit.dll"
#load @"MbUnitTestTypes.fs"
#load @"MbUnitTests.fs"

open System
open Fuchu

MbUnitTests.tests
|> Test.filter (fun n -> n.Contains "StaticTestFactory")
|> run