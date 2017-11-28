#r @"packages/build/FAKE/tools/FakeLib.dll"

open System
open Fake

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = IO.File.ReadAllLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes

Target "Clean" (fun _ ->
    !!"./**/bin/" ++ "./**/obj/" |> CleanDirs
)

open AssemblyInfoFile
Target "AssemblyInfo" (fun _ ->

    [ "Expecto"
      "Expecto.BenchmarkDotNet"
      "Expecto.FsCheck"
    ]
    |> List.iter (fun product ->
        [ Attribute.Title product
          Attribute.Product product
          Attribute.Copyright "Copyright \169 2017"
          Attribute.Description "Advanced testing library for F#"
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion
        ] |> CreateFSharpAssemblyInfo (product+"/AssemblyInfo.fs")
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
    Shell.Exec ("Expecto.Tests/bin/"+configuration+"/Expecto.Tests.exe","--summary")
    |> fun r -> if r<>0 then failwith "Expecto.Tests.exe failed"
)

Target "TestCSharp" (fun _ ->
    Shell.Exec ("Expecto.Tests.CSharp/bin/"+configuration+"/Expecto.Tests.CSharp.exe","--summary")
    |> fun r -> if r<>0 then failwith "Expecto.Tests.CSharp.exe failed"
)

Target "Pack" (fun _ ->
    Paket.Pack (fun p ->
      { p with
          OutputPath = "bin"
          Version = release.AssemblyVersion
          ReleaseNotes = toLines release.Notes
      })
)

Target "DotNetCoreRestore" (fun _ ->
    DotNetCli.Restore (fun p ->
    { p with
        Project = "Expecto/Expecto.netcore.fsproj"
    })
)

Target "DotNetCoreBuild" (fun _ ->
    DotNetCli.Build (fun p ->
    { p with
        Configuration = configuration
        Project = "Expecto/Expecto.netcore.fsproj"
    })
)

Target "DotNetCoreRestoreFsCheck" (fun _ ->
    DotNetCli.Restore (fun p ->
    { p with
        Project = "Expecto.FsCheck/Expecto.FsCheck.netcore.fsproj"
    })
)

Target "DotNetCoreBuildFsCheck" (fun _ ->
    DotNetCli.Build (fun p ->
    { p with
        Configuration = configuration
        Project = "Expecto.FsCheck/Expecto.FsCheck.netcore.fsproj"
    })
)

Target "DotNetCoreRestoreBenchmarkDotNet" (fun _ ->
    DotNetCli.Restore (fun p ->
    { p with
        Project = "Expecto.BenchmarkDotNet/Expecto.BenchmarkDotNet.netcore.fsproj"
    })
)

Target "DotNetCoreBuildBenchmarkDotNet" (fun _ ->
    DotNetCli.Build (fun p ->
    { p with
        Configuration = configuration
        Project = "Expecto.FsCheck/Expecto.BenchmarkDotNet.netcore.fsproj"
    })
)

Target "DotNetCoreRestoreTest" (fun _ ->
    DotNetCli.Restore (fun p ->
    { p with
        Project = "Expecto.Tests/Expecto.Tests.netcore.fsproj"
    })
)

Target "DotNetCoreBuildTest" (fun _ ->
    DotNetCli.Build (fun p ->
    { p with
        Configuration = configuration
        Project = "Expecto.Tests/Expecto.Tests.netcore.fsproj"
    })
)

Target "DotNetCoreRunTestNet461" (fun _ ->
    DotNetCli.RunCommand id ("Expecto.Tests/bin/"+configuration+"/net461/Expecto.Tests.exe --summary")
)

Target "DotNetCoreRunTest11" (fun _ ->
    DotNetCli.RunCommand id ("Expecto.Tests/bin/"+configuration+"/netcoreapp1.1/Expecto.Tests.dll --summary")
)

Target "DotNetCoreRunTest20" (fun _ ->
    DotNetCli.RunCommand id ("Expecto.Tests/bin/"+configuration+"/netcoreapp2.0/Expecto.Tests.dll --summary")
)

Target "DotNetCorePack" (fun _ ->
    DotNetCli.Pack (fun p ->
    { p with
        Project = "Expecto/Expecto.netcore.fsproj"
        Configuration = configuration
        OutputPath = "bin"
    })
)

Target "DotNetCorePackFsCheck" (fun _ ->
    DotNetCli.Pack (fun p ->
    { p with
        Project = "Expecto.FsCheck/Expecto.FsCheck.netcore.fsproj"
        Configuration = configuration
        OutputPath = "bin"
    })
)

Target "Merge" (fun _ ->
    DotNetCli.Restore (fun p -> { p with Project = "tools/tools.proj" })
    DotNetCli.RunCommand (fun p -> { p with WorkingDir = "tools" })
        ("mergenupkg --source ../bin/Expecto."+release.NugetVersion+".nupkg --other ../Expecto/bin/Expecto.netcore.1.0.0.nupkg --framework netstandard1.6")
    DotNetCli.RunCommand (fun p -> { p with WorkingDir = "tools" })
        ("mergenupkg --source ../bin/Expecto."+release.NugetVersion+".nupkg --other ../Expecto/bin/Expecto.netcore.1.0.0.nupkg --framework netstandard2.0")
    DotNetCli.RunCommand (fun p -> { p with WorkingDir = "tools" })
        ("mergenupkg --source ../bin/Expecto.FsCheck."+release.NugetVersion+".nupkg --other ../Expecto.FsCheck/bin/Expecto.FsCheck.netcore.1.0.0.nupkg --framework netstandard1.6")
    DotNetCli.RunCommand (fun p -> { p with WorkingDir = "tools" })
        ("mergenupkg --source ../bin/Expecto.FsCheck."+release.NugetVersion+".nupkg --other ../Expecto.FsCheck/bin/Expecto.FsCheck.netcore.1.0.0.nupkg --framework netstandard2.0")
)

Target "Push" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = "bin" }))

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
Target "Release" (fun _ ->
    let gitOwner = "haf"
    let gitName = "expecto"
    let gitOwnerName = gitOwner + "/" + gitName
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.tryFind (fun s -> s.EndsWith "(push)" && s.Contains gitOwnerName)
        |> function None -> ("ssh://github.com/"+gitOwnerName) | Some s -> s.Split().[0]

    Git.Staging.StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

    let user = getUserInput "Github Username: "
    let pw = getUserPassword "Github Password: "

    Octokit.createClient user pw
    |> Octokit.createDraft gitOwner gitName release.NugetVersion
        (Option.isSome release.SemVer.PreRelease) release.Notes
    |> Octokit.releaseDraft
    |> Async.RunSynchronously
)

Target "Initialize" ignore
Target "Framework" ignore
Target "DotNetCore" ignore
Target "All" ignore

"Clean"
==> "AssemblyInfo"
==> "PaketFiles"
==> "Initialize"

"Initialize"
==> "Build"
==> "Test"
==> "TestCSharp"
==> "Pack"
==> "Framework"

"Initialize"
==> "DotNetCoreRestore"
==> "DotNetCoreBuild"
==> "DotNetCoreRestoreFsCheck"
==> "DotNetCoreBuildFsCheck"
//==> "DotNetCoreRestoreBenchmarkDotNet"
//==> "DotNetCoreBuildBenchmarkDotNet"
==> "DotNetCoreRestoreTest"
==> "DotNetCoreBuildTest"
//==> "DotNetCoreRunTestNet461"
==> "DotNetCoreRunTest11"
==> "DotNetCoreRunTest20"
==> "DotNetCorePack"
==> "DotNetCorePackFsCheck"
//==> "DotNetCorePackBenchmarkDotNet"
==> "DotNetCore"

"Framework" ==> "Merge"
"DotNetCore" ==> "Merge"
"Merge" ==> "All"

"All"
==> "Push"
==> "Release"

RunTargetOrDefault "All"