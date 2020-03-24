#r "paket: groupref Build //"

#load "./.fake/build.fsx/intellisense.fsx"
#load "paket-files/build/eiriktsarpalis/snippets/SlnTools/SlnTools.fs"
#if !FAKE
#r "netstandard"
#r "facades/netstandard"
#endif

open System
open System.IO
open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.Api
open Fake.IO
open Fake.Core.TargetOperators
open Fake.IO.Globbing.Operators
open Fake.BuildServer

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let configuration =
  Environment.environVarOrDefault "CONFIGURATION" "Release"
  |> DotNet.BuildConfiguration.fromString

let release = ReleaseNotes.load "RELEASE_NOTES.md"
let description = "Advanced testing library for F#"
let tags = "test testing fsharp assert expect stress performance unit random property"
let authors = "Anthony Lloyd & Henrik Feldt and contributors"
let owners = "Anthony Lloyd & Henrik Feldt (cloned from Fuchu by @mausch)"
let projectUrl = "https://github.com/haf/expecto"
let iconUrl = "https://raw.githubusercontent.com/haf/expecto/master/docs/expecto-logo-small.png"
let licenceUrl = "https://github.com/haf/expecto/blob/master/LICENSE"
let copyright = "Copyright 2019"
let mutable dotnetExePath = "dotnet"

let envRequired k =
  let v = Environment.GetEnvironmentVariable k
  if isNull v then failwithf "Missing environment key '%s'." k
  v

BuildServer.install [
  Travis.Installer
  AppVeyor.Installer
]

let libProjects =
  !! "Expecto/*.fsproj"
  ++ "Expecto.FsCheck/*.fsproj"
  ++ "Expecto.Diff/*.fsproj"
  ++ "Expecto.BenchmarkDotNet/*.fsproj"
  ++ "Expecto.Hopac/*.fsproj"

let testProjects =
  !! "Expecto.Tests/*.Tests.fsproj"
  ++ "Expecto.Hopac.Tests/*.Tests.fsproj"
  ++ "Expecto.Tests.CSharp/*.Tests.CSharp.csproj"
  ++ "Expecto.Focused.Tests/*.Tests.fsproj"

let benchmarkProjects =
  !! "Expecto.BenchmarkDotNet/*.fsproj"
  ++ "Expecto.BenchmarkDotNet.Tests/*.fsproj"

let pkgPath = Path.GetFullPath "./pkg"

Target.create "Clean" <| fun _ ->
  !!"./**/bin/"
  ++ "./**/obj/"
  |> Shell.cleanDirs

let normaliseFileToLFEnding filename =
    let s = File.readAsString filename
    s.Replace(String.WindowsLineBreaks,String.LinuxLineBreaks)
    |> File.writeString false filename

Target.create "AssemblyInfo" (fun _ ->
    let createAssemblyInfo project =
        let filename = project+"/AssemblyInfo.fs"
        AssemblyInfoFile.createFSharpWithConfig filename [
            AssemblyInfo.Title project
            AssemblyInfo.Product project
            AssemblyInfo.Copyright copyright
            AssemblyInfo.Description description
            AssemblyInfo.Version release.AssemblyVersion
            AssemblyInfo.FileVersion release.AssemblyVersion
            AssemblyInfo.InternalsVisibleTo "Expecto.Tests"
        ] (AssemblyInfoFileConfig(true,false,"Expecto.AssemblyInfo"))
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
    { p with Configuration = configuration
             Common = DotNet.Options.withDotNetCliPath dotnetExePath p.Common })
    project

Target.create "BuildExpecto" (fun _ ->
  build (SlnTools.createTempSolutionFile libProjects)
)

Target.create "BuildBenchmarkDotNet" (fun _ ->
  build (SlnTools.createTempSolutionFile benchmarkProjects)
)

Target.create "BuildTest" (fun _ ->
  build (SlnTools.createTempSolutionFile testProjects)
)

Target.create "RunTest" <| fun _ ->

    let runTest project =
        DotNet.exec (DotNet.Options.withDotNetCliPath dotnetExePath)
             (sprintf "%s/bin/%O/netcoreapp2.1/%s.dll" project configuration project)
             "--summary"
        |> fun r -> if r.ExitCode<>0 then project+".dll failed" |> failwith
        let exeName = sprintf "%s/bin/%O/net461/%s.exe" project configuration project
        let filename, arguments =
            let args = "--colours 0 --summary"
            if Environment.isWindows then exeName, args
            else "mono", exeName + " " + args
        Process.shellExec
          { ExecParams.Empty with
              Program = filename
              CommandLine = arguments
          }
        |> fun r -> if r<>0 then project+".exe failed" |> failwith

    runTest "Expecto.Tests"

    "Expecto.Tests.TestResults.xml"
    |> Path.combine (Path.combine __SOURCE_DIRECTORY__ "bin")
    |> Trace.publish (ImportData.Nunit NunitDataVersion.Nunit)

    runTest "Expecto.Tests.CSharp"

    "Expecto.Tests.CSharp.TestResults.xml"
    |> Path.combine (Path.combine __SOURCE_DIRECTORY__ "bin")
    |> Trace.publish (ImportData.Nunit NunitDataVersion.Nunit)

    runTest "Expecto.Hopac.Tests"
    runTest "Expecto.Focused.Tests"

Target.create "Pack" <| fun _ ->
  let args =
    { MSBuild.CliArguments.Create() with
        NoLogo = true
        DoRestore = false
        Properties =
          [ "PackageVersion", release.NugetVersion
            "Authors", authors
            "Owners", owners
            "PackageRequireLicenseAcceptance", "true"
            "Description", description.Replace(",","")
            "PackageReleaseNotes", (release.Notes |> String.toLines).Replace(",","").Replace(";", "—")
            "Copyright", copyright
            "PackageTags", tags
            "PackageProjectUrl", projectUrl
            "PackageIconUrl", iconUrl
            "PackageLicenseUrl", licenceUrl
          ]
    }
  let pkgSln = SlnTools.createTempSolutionFile libProjects
  let setParams (p: DotNet.PackOptions) =
    { p with
        OutputPath = Some pkgPath
        Configuration = configuration
        MSBuildParams = args }
  DotNet.pack setParams pkgSln

Target.create "Push" <| fun _ ->
  let setParams (p: Paket.PaketPushParams) =
    { p with
        ToolPath = Path.GetFullPath "./.paket/paket"
        WorkingDir = pkgPath
        ApiKey = envRequired "NUGET_TOKEN" }
  // for f in *.nupkg; do ../.paket/paket push  --api-key $NUGET_TOKEN $f; done
  Paket.push setParams

Target.create "CheckEnv" <| fun _ ->
  ignore (envRequired "GITHUB_TOKEN")
  ignore (envRequired "NUGET_TOKEN")

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

"CheckEnv"
  ==> "Release"

"Clean"
  ==> "AssemblyInfo"
  ==> "ProjectVersion"
  ==> "BuildExpecto"
  ==> "BuildBenchmarkDotNet"
  ==> "BuildTest"
  ==> "RunTest"
  ==> "Pack"
  ==> "All"
  ==> "Push"
  ==> "Release"

Target.runOrDefaultWithArguments "All"
