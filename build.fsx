#r @"packages/build/FAKE/tools/FakeLib.dll"

open System
open Fake

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let configuration = environVarOrDefault "Configuration" "Release"
let release = IO.File.ReadAllLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes
let description = "Advanced testing library for F#"
let tags = "test testing fsharp assert expect stress performance unit random property"
let authors = "Anthony Lloyd, Henrik Feldt and contributors"
let owners = "Anthony Lloyd, Henrik Feldt (cloned from Fuchu by @mausch)"
let projectUrl = "https://github.com/haf/expecto"
let iconUrl = "https://raw.githubusercontent.com/haf/expecto/master/docs/expecto-logo-small.png"
let licenceUrl = "https://github.com/haf/expecto/blob/master/LICENSE"
let copyright = "Copyright 2018"
let mutable dotnetExePath = "dotnet"

Target "InstallDotNetCore" (fun _ ->
    dotnetExePath <- DotNetCli.InstallDotNetSDK "2.0.3"
)

Target "Clean" (fun _ -> !!"./**/bin/" ++ "./**/obj/" |> CleanDirs)

open Fake.StringHelper
let normaliseFileToLFEnding filename =
    let s = ReadFileAsString filename
    s.Replace(WindowsLineBreaks,LinuxLineBreaks)
    |> WriteStringToFile false filename

open AssemblyInfoFile
Target "AssemblyInfo" (fun _ ->
    let createAssemblyInfo project =
        let filename = project+"/AssemblyInfo.fs"
        CreateFSharpAssemblyInfo filename [
            Attribute.Title project
            Attribute.Product project
            Attribute.Copyright copyright
            Attribute.Description description
            Attribute.Version release.AssemblyVersion
            Attribute.FileVersion release.AssemblyVersion
        ]
        normaliseFileToLFEnding filename
    createAssemblyInfo "Expecto"
    createAssemblyInfo "Expecto.FsCheck"
    createAssemblyInfo "Expecto.BenchmarkDotNet"
    createAssemblyInfo "Expecto.Hopac"
)

Target "PaketFiles" (fun _ ->
    let filename = "paket-files/logary/logary/src/Logary.Facade/Facade.fs"
    FileHelper.ReplaceInFiles ["namespace Logary.Facade","namespace Expecto.Logging"]
        [filename]
    normaliseFileToLFEnding filename
)

Target "ProjectVersion" (fun _ ->
    let setProjectVersion project =
        let filename = project+"/"+project+".fsproj"
        XMLHelper.XmlPoke filename
            "Project/PropertyGroup/Version/text()" release.NugetVersion
        normaliseFileToLFEnding filename
    setProjectVersion "Expecto"
    setProjectVersion "Expecto.FsCheck"
    setProjectVersion "Expecto.BenchmarkDotNet"
    setProjectVersion "Expecto.Hopac"
)
let build project =
    DotNetCli.Build (fun p ->
    { p with
        ToolPath = dotnetExePath
        Configuration = configuration
        Project = project
    })

Target "BuildBenchmarkDotNet" (fun _ ->
    build "Expecto.BenchmarkDotNet/Expecto.BenchmarkDotNet.fsproj"
)

Target "BuildTest" (fun _ ->
    build "Expecto.Tests/Expecto.Tests.fsproj"
    build "Expecto.Hopac.Tests/Expecto.Hopac.Tests.fsproj"
    build "Expecto.Tests.CSharp/Expecto.Tests.CSharp.csproj"
    build "Expecto.Focused.Tests/Expecto.Focused.Tests.fsproj"
)

Target "RunTest" (fun _ ->
    let runTest project =
        DotNetCli.RunCommand (fun p -> { p with ToolPath = dotnetExePath })
            (project+"/bin/"+configuration+"/netcoreapp2.0/"+project+".dll --summary")
        Shell.Exec (project+"/bin/"+configuration+"/net461/"+project+".exe","--summary")
        |> fun r -> if r<>0 then project+".exe failed" |> failwith
    runTest "Expecto.Tests"
    runTest "Expecto.Hopac.Tests"
    runTest "Expecto.Tests.CSharp"
    runTest "Expecto.Focused.Tests"
)

Target "Pack" (fun _ ->
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
                sprintf "/p:PackageReleaseNotes=\"%O\"" ((toLines release.Notes).Replace(",",""))
                sprintf "/p:Copyright=\"%s\"" copyright
                sprintf "/p:PackageTags=\"%s\"" tags
                sprintf "/p:PackageProjectUrl=\"%s\"" projectUrl
                sprintf "/p:PackageIconUrl=\"%s\"" iconUrl
                sprintf "/p:PackageLicenseUrl=\"%s\"" licenceUrl
            ] |> String.concat " "
        "pack "+project+"/"+project+".fsproj -c "+configuration + " -o ../bin " + packParameters
        |> DotNetCli.RunCommand id
    pack "Expecto"
    pack "Expecto.FsCheck"
    pack "Expecto.BenchmarkDotNet"
    pack "Expecto.Hopac"
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

Target "All" ignore

"Clean"
==> "InstallDotNetCore"
==> "AssemblyInfo"
==> "PaketFiles"
==> "ProjectVersion"
==> "BuildBenchmarkDotNet"
==> "BuildTest"
==> "RunTest"
==> "Pack"
==> "All"
==> "Push"
==> "Release"

RunTargetOrDefault "All"
