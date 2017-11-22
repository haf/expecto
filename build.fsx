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

let configuration = environVarOrDefault "Configuration" "Release"

Target "Build" (fun _ ->
    { BaseDirectory = __SOURCE_DIRECTORY__
      Includes = ["Expecto.sln"]
      Excludes = [] } 
    |> MSBuild "" "Build" ["Configuration", configuration]
    |> Log "Compile-Output: "
)

Target "Test" (fun _ ->
    Shell.Exec ("Expecto.Tests/bin/"+configuration+"/Expecto.Tests.exe")
    |> fun r -> if r<>0 then failwith "Expecto.Tests.exe failed"
)

Target "TestCSharp" (fun _ ->
    Shell.Exec ("Expecto.Tests.CSharp/bin/"+configuration+"/Expecto.Tests.CSharp.exe")
    |> fun r -> if r<>0 then failwith "Expecto.Tests.CSharp.exe failed"
)

Target "DotNetCoreRestore" (fun _ ->
    DotNetCli.Restore (fun p ->
        { p with
            Project = "Expecto.netcore/Expecto.netcore.fsproj"
        })
)

Target "DotNetCoreBuild" (fun _ ->
    DotNetCli.Build (fun p ->
    { p with
        Configuration = configuration
        Project = "Expecto.netcore/Expecto.netcore.fsproj"
    })
)

Target "DotNetCoreRestoreTest" (fun _ ->
    DotNetCli.Restore (fun p ->
        { p with
            Project = "Expecto.netcore.Tests/Expecto.netcore.Tests.fsproj"
        })
)

Target "DotNetCoreBuildTest" (fun _ ->
    DotNetCli.Build (fun p ->
    { p with
        Configuration = configuration
        Project = "Expecto.netcore.Tests/Expecto.netcore.Tests.fsproj"
    })
)

Target "DotNetCoreRunTest" (fun _ ->
    DotNetCli.RunCommand id (" ./Expecto.netcore.Tests/bin/"+configuration+"/netcoreapp1.1/Expecto.netcore.Tests.dll")
)

// Target "NuGet" <| fun () ->
//     NuGet (fun p -> 
//         { p with
//             // Authors = authors
//             // Project = project
//             // Summary = summary
//             // Description = description
//             // Version = nugetVersion
//             ReleaseNotes = toLines release.Notes
//             //Tags = tags
//             OutputPath = "bin"
//             AccessKey = getBuildParamOrDefault "nugetkey" ""
//             Publish = hasBuildParam "nugetkey"
//             Dependencies = [] })
//         "nuget/Expecto.nuspec"

Target "All" ignore

"AssemblyInfo"
==> "PaketFiles"
==> "Build"
==> "Test"
==> "TestCSharp"
==> "DotNetCoreRestore"
==> "DotNetCoreBuild"
==> "DotNetCoreRestoreTest"
==> "DotNetCoreBuildTest"
==> "DotNetCoreRunTest"
==> "All"

RunTargetOrDefault "All"