#r @"packages/build/FAKE/tools/FakeLib.dll"

open System
open Fake

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = IO.File.ReadAllLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes

open AssemblyInfoFile
Target "AssemblyInfo" (fun _ ->

    let create product =
        [ 
            Attribute.Product product
            Attribute.Copyright "\169 Anthony Lloyd (formerly Henrik Feldt and cloned from Fuchu by @mausch)"
            Attribute.Description "A smooth unit test framework for F#"
            Attribute.Version release.AssemblyVersion
            Attribute.FileVersion release.AssemblyVersion
        ] |> CreateFSharpAssemblyInfo ("src/"+product+"/AssemblyInfo.fs")

    create "Expecto"
    create "Expecto.BenchmarkDotNet"
    create "Expecto.FsCheck"
)

Target "PaketFiles" (fun _ ->
    FileHelper.ReplaceInFiles ["namespace Logary.Facade","namespace Expecto.Logging"]
        ["paket-files/logary/logary/src/Logary.Facade/Facade.fs"]
)

"AssemblyInfo" ==> "PaketFiles"

RunTargetOrDefault "PaketFiles"