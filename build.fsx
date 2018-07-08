#r "paket:
nuget Fake.Core.Xml
nuget Fake.DotNet.Cli
nuget Fake.DotNet.Paket
nuget Fake.Tools.Git
nuget Fake.Api.GitHub
nuget Fake.Core.Target
nuget Fake.Core.Environment
nuget Fake.Core.UserInput
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.Core.ReleaseNotes //"
#load "./.fake/build.fsx/intellisense.fsx"
#if !FAKE
#r "netstandard"
#endif

open System
open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.Api
open Fake.IO
open Fake.Core.TargetOperators
open Fake.IO.Globbing.Operators

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let configuration = Environment.environVarOrDefault "Configuration" "Release"
let release = ReleaseNotes.load "RELEASE_NOTES.md"
let description = "Advanced testing library for F#"
let tags = "test testing fsharp assert expect stress performance unit random property"
let authors = "Anthony Lloyd & Henrik Feldt and contributors"
let owners = "Anthony Lloyd & Henrik Feldt (cloned from Fuchu by @mausch)"
let projectUrl = "https://github.com/haf/expecto"
let iconUrl = "https://raw.githubusercontent.com/haf/expecto/master/docs/expecto-logo-small.png"
let licenceUrl = "https://github.com/haf/expecto/blob/master/LICENSE"
let copyright = "Copyright 2018"
let mutable dotnetExePath = "dotnet"

Target.create "Clean" (fun _ ->
    !!"./**/bin/" ++ "./**/obj/" |> Shell.cleanDirs
)

let normaliseFileToLFEnding filename =
    let s = File.readAsString filename
    s.Replace(String.WindowsLineBreaks,String.LinuxLineBreaks)
    |> File.writeString false filename

Target.create "AssemblyInfo" (fun _ ->
    let createAssemblyInfo project =
        let filename = project+"/AssemblyInfo.fs"
        AssemblyInfoFile.createFSharp filename [
            AssemblyInfo.Title project
            AssemblyInfo.Product project
            AssemblyInfo.Copyright copyright
            AssemblyInfo.Description description
            AssemblyInfo.Version release.AssemblyVersion
            AssemblyInfo.FileVersion release.AssemblyVersion
        ]
        normaliseFileToLFEnding filename
    createAssemblyInfo "Expecto"
    createAssemblyInfo "Expecto.FsCheck"
    createAssemblyInfo "Expecto.BenchmarkDotNet"
    createAssemblyInfo "Expecto.Hopac"
)

Target.create "ProjectVersion" (fun _ ->
    let setProjectVersion project =
        let filename = project+"/"+project+".fsproj"
        Xml.pokeInnerText filename
            "Project/PropertyGroup/Version" release.NugetVersion
        normaliseFileToLFEnding filename
    setProjectVersion "Expecto"
    setProjectVersion "Expecto.FsCheck"
    setProjectVersion "Expecto.BenchmarkDotNet"
    setProjectVersion "Expecto.Hopac"
)
let build project =
    DotNet.build (fun p ->
    { p with
        Configuration = DotNet.BuildConfiguration.Custom configuration
        Common = DotNet.Options.withDotNetCliPath dotnetExePath p.Common
                 |> DotNet.Options.withCustomParams (Some "--no-dependencies")
    }) project

Target.create "BuildExpecto" (fun _ ->
    build "Expecto/Expecto.fsproj"
    build "Expecto.Hopac/Expecto.Hopac.fsproj"
    build "Expecto.FsCheck/Expecto.FsCheck.fsproj"
)

Target.create "BuildBenchmarkDotNet" (fun _ ->
    build "Expecto.BenchmarkDotNet/Expecto.BenchmarkDotNet.fsproj"
    build "Expecto.BenchmarkDotNet.Tests/Expecto.BenchmarkDotNet.Tests.fsproj"
)

Target.create "BuildTest" (fun _ ->
    build "Expecto.Tests/Expecto.Tests.fsproj"
    build "Expecto.Hopac.Tests/Expecto.Hopac.Tests.fsproj"
    build "Expecto.Tests.CSharp/Expecto.Tests.CSharp.csproj"
    build "Expecto.Focused.Tests/Expecto.Focused.Tests.fsproj"
)

Target.create "RunTest" (fun _ ->
    let runTest project =
        DotNet.exec (DotNet.Options.withDotNetCliPath dotnetExePath)
             (project+"/bin/"+configuration+"/netcoreapp2.0/"+project+".dll")
             "--summary"
        |> fun r -> if r.ExitCode<>0 then project+".dll failed" |> failwith
        let exeName = project+"/bin/"+configuration+"/net461/"+project+".exe"
        let filename, arguments =
            if Environment.isWindows then exeName, "--summary"
            else "mono", exeName + " --summary"
        Process.execSimple (fun si ->
          { si with
              FileName = filename
              Arguments = arguments
          }
        ) TimeSpan.MaxValue
        |> fun r -> if r<>0 then project+".exe failed" |> failwith
    runTest "Expecto.Tests"
    runTest "Expecto.Hopac.Tests"
    runTest "Expecto.Tests.CSharp"
    runTest "Expecto.Focused.Tests"
)

Target.create "Pack" (fun _ ->
    let pack project =
        let packParameters =
            [
                "--no-build"
                "--no-restore"
                sprintf "/p:Title=\"%s\"" project
                "/p:PackageVersion=" + release.NugetVersion
                sprintf "/p:Authors=\"%s\"" authors
                sprintf "/p:Owners=\"%s\"" owners
                "/p:PackageRequireLicenseAcceptance=false"
                sprintf "/p:Description=\"%s\"" description
                sprintf "/p:PackageReleaseNotes=\"%O\""
                    ((String.toLines release.Notes).Replace(",",""))
                sprintf "/p:Copyright=\"%s\"" copyright
                sprintf "/p:PackageTags=\"%s\"" tags
                sprintf "/p:PackageProjectUrl=\"%s\"" projectUrl
                sprintf "/p:PackageIconUrl=\"%s\"" iconUrl
                sprintf "/p:PackageLicenseUrl=\"%s\"" licenceUrl
            ] |> String.concat " "
        "pack "+project+"/"+project+".fsproj -c "+configuration + " -o ../bin "
        + packParameters
        |> DotNet.exec id <| ""
        |> ignore
    pack "Expecto"
    pack "Expecto.FsCheck"
    pack "Expecto.BenchmarkDotNet"
    pack "Expecto.Hopac"
)

Target.create "Push" (fun _ ->
    Paket.push (fun p -> { p with WorkingDir = "bin" })
)

Target.create "Release" (fun _ ->
    let gitOwner = "haf"
    let gitName = "expecto"
    let gitOwnerName = gitOwner + "/" + gitName
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.tryFind (fun s -> s.EndsWith "(push)" && s.Contains gitOwnerName)
        |> function | None -> "ssh://github.com/" + gitOwnerName
                    | Some s -> s.Split().[0]

    Git.Staging.stageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

    Environment.environVar "GITHUB_TOKEN"
    |> GitHub.createClientWithToken
    |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion
                              release.SemVer.PreRelease.IsSome release.Notes
    |> GitHub.publishDraft
    |> Async.RunSynchronously
)

Target.create "All" ignore

"Clean"
==> "AssemblyInfo"
==> "ProjectVersion"
==> "BuildExpecto"
//==> "BuildBenchmarkDotNet"
==> "BuildTest"
==> "RunTest"
==> "Pack"
==> "All"
==> "Push"
==> "Release"

Target.runOrDefaultWithArguments "All"
