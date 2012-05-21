#r @"..\lib\NUnit.Framework.dll"
#load @"..\Fuchu\Fuchu.fs"
#load @"..\Fuchu\Fuchu.NUnit.fs"
#load @"NUnitTestTypes.fs"
#load @"NUnitTests.fs"

open System
open Fuchu

run NUnitTests.tests