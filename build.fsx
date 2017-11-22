#r @"packages/build/FAKE/tools/FakeLib.dll"

open System
open Fake

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = IO.File.ReadAllLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes

open AssemblyInfoFile
Target "AssemblyInfo" (fun _ ->

    [ "Expecto"
      "Expecto.BenchmarkDotNet"
      "Expecto.FsCheck"
    ]
    |> List.iter (fun product ->
        [ Attribute.Product product
          Attribute.Copyright "\169 Anthony Lloyd (formerly Henrik Feldt and cloned from Fuchu by @mausch)"
          Attribute.Description "A smooth unit test framework for F#"
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion
        ] |> CreateFSharpAssemblyInfo (product+"/AssemblyVersionInfo.fs")
    )
)

Target "PaketFiles" (fun _ ->
    FileHelper.ReplaceInFiles ["namespace Logary.Facade","namespace Expecto.Logging"]
        ["paket-files/logary/logary/src/Logary.Facade/Facade.fs"]
)

Target "InstallDotNetCore" (fun _ ->
    if DotNetCli.isInstalled() then
        DotNetCli.getVersion() |> logfn "DotNetCore %s already installed"
    else
        DotNetCli.InstallDotNetSDK "1.1.5" |> logfn "DotNetCore installed to: %s"
        DotNetCli.DotnetSDKPath |> logfn "DotnetSDKPath: %s"
)

Target "Restore" (fun _ ->
    DotNetCli.Restore (fun p ->
        { p with
            ToolPath = DotNetCli.DotnetSDKPath+"/dotnet"
            Project = "Expecto.netcore/Expecto.netcore.fsproj"
        })
)

let configuration = environVarOrDefault "Configuration" "Release"

Target "Compile" (fun _ ->
    { BaseDirectory = __SOURCE_DIRECTORY__
      Includes = ["Expecto.sln"]
      Excludes = [] } 
    |> MSBuild "" "Build" ["Configuration", configuration]
    |> Log "Compile-Output: "

    // DotNetCli.Build (fun p ->
    // { p with
    //     Configuration = configuration
    //     Project = "Expecto.netcore/Expecto.netcore.fsproj"
    // })
)



// Target "Build" (fun _ -> DotNetCli.Build (fun p -> {p with Configuration = "Release"}))

Target "Test" (fun _ ->
    Shell.Exec ("Expecto.Tests/bin/"+configuration+"/Expecto.Tests.exe")
    |> fun r -> if r<>0 then failwith "Expecto.Tests.exe failed"
    Shell.Exec ("Expecto.Tests.CSharp/bin/"+configuration+"/Expecto.Tests.CSharp.exe")
    |> fun r -> if r<>0 then failwith "Expecto.Tests.CSharp.exe failed"
)

"AssemblyInfo"
==> "PaketFiles"
==> "InstallDotNetCore"
==> "Restore"
==> "Compile"
==> "Test"

RunTargetOrDefault "Test"