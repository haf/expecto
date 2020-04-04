#r "paket: groupref Build //"

#load "./.fake/build.fsx/intellisense.fsx"
#load "paket-files/build/eiriktsarpalis/snippets/SlnTools/SlnTools.fs"
#if !FAKE
#r "netstandard"
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
let testFramework = "netcoreapp3.1"
let dotnetExePath = "dotnet"

let githubToken = lazy(Environment.environVarOrFail "GITHUB_TOKEN")
let nugetToken = lazy(Environment.environVarOrFail "NUGET_TOKEN")

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
  ++ pkgPath
  |> Shell.cleanDirs

let normaliseFileToLFEnding filename =
    let s = File.readAsString filename
    s.Replace(String.WindowsLineBreaks,String.LinuxLineBreaks)
    |> File.writeString false filename

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
    Trace.logfn "Running %s on .NET Core" project
    DotNet.exec (DotNet.Options.withDotNetCliPath dotnetExePath)
      (sprintf "%s/bin/%O/%s/%s.dll" project configuration testFramework project)
      "--summary"
    |> fun r -> if r.ExitCode <> 0 then failwithf "Running %s on .NET Core failed" project

    Trace.logfn "Running %s on .NET Framework" project
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
    |> fun r -> if r <> 0 then failwithf "Running %s on .NET Framework failed" project

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
        Properties = [
          "Version", release.NugetVersion
          "PackageReleaseNotes", String.toLines release.Notes
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
        WorkingDir = pkgPath
        ApiKey = nugetToken.Value }
  // for f in *.nupkg; do dotnet paket push  --api-key $NUGET_TOKEN $f; done
  Paket.push setParams

Target.create "CheckEnv" <| fun _ ->
  ignore nugetToken.Value
  ignore githubToken.Value

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

    githubToken.Value
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
  ==> "BuildExpecto"
  ==> "BuildBenchmarkDotNet"
  ==> "BuildTest"
  ==> "RunTest"
  ==> "Pack"
  ==> "All"
  ==> "Push"
  ==> "Release"

Target.runOrDefaultWithArguments "All"
