#r @"packages/build/FAKE/tools/FakeLib.dll"

open System
open Fake

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let configuration = environVarOrDefault "Configuration" "Release"
let release = IO.File.ReadAllLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes
let description = "Advanced testing library for F#"
let tags = "test testing fsharp assert expect stress performance unit random property"
let authors = "Anthony Lloyd and contributors"
let owners = "Anthony Lloyd (formerly Henrik Feldt and cloned from Fuchu by @mausch)"
let projectUrl = "https://github.com/haf/expecto"
let iconUrl = "https://raw.githubusercontent.com/haf/expecto/master/docs/expecto-logo-small.png"
let licenceUrl = "https://github.com/haf/expecto/blob/master/LICENSE"
let copyright = "Copyright \169 2018"
let mutable dotnetExePath = "dotnet"

Target "InstallDotNetCore" (fun _ ->
    dotnetExePath <- DotNetCli.InstallDotNetSDK "2.0.3"
)

Target "Clean" (fun _ -> !!"./**/bin/" ++ "./**/obj/" |> CleanDirs)

open AssemblyInfoFile
Target "AssemblyInfo" (fun _ ->

    [ "Expecto"
      "Expecto.FsCheck"
      "Expecto.BenchmarkDotNet"
      "Expecto.Hopac"
    ]
    |> List.iter (fun product ->
        [ Attribute.Title product
          Attribute.Product product
          Attribute.Copyright copyright
          Attribute.Description description
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion
        ] |> CreateFSharpAssemblyInfo (product+"/AssemblyInfo.fs")
    )
)

Target "PaketFiles" (fun _ ->
    FileHelper.ReplaceInFiles ["namespace Logary.Facade","namespace Expecto.Logging"]
        ["paket-files/logary/logary/src/Logary.Facade/Facade.fs"]
)

Target "ProjectVersion" (fun _ ->
    [
        "Expecto/Expecto.fsproj"
        "Expecto.FsCheck/Expecto.FsCheck.fsproj"
        "Expecto.BenchmarkDotNet/Expecto.BenchmarkDotNet.fsproj"
        "Expecto.Hopac/Expecto.Hopac.fsproj"
    ]
    |> List.iter (fun file ->
        XMLHelper.XmlPoke file "Project/PropertyGroup/Version/text()" release.NugetVersion)
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
    build "Expecto.Tests.CSharp/Expecto.Tests.CSharp.csproj"
    build "Expecto.Focused.Tests/Expecto.Focused.Tests.fsproj"
)
let run f = DotNetCli.RunCommand (fun p -> { p with ToolPath = dotnetExePath } |> f)

Target "RunTest" (fun _ ->
    run id ("Expecto.Tests/bin/"+configuration+"/netcoreapp2.0/Expecto.Tests.dll --summary")
    Shell.Exec ("Expecto.Tests/bin/"+configuration+"/net461/Expecto.Tests.exe","--summary")
    |> fun r -> if r<>0 then failwith "Expecto.Tests.exe failed"
    if EnvironmentHelper.isWindows then
        run id ("Expecto.Tests/bin/"+configuration+"/netcoreapp1.1/Expecto.Tests.dll --summary")

    run id ("Expecto.Tests.CSharp/bin/"+configuration+"/netcoreapp2.0/Expecto.Tests.CSharp.dll --summary")
    Shell.Exec ("Expecto.Tests.CSharp/bin/"+configuration+"/net461/Expecto.Tests.CSharp.exe","--summary")
    |> fun r -> if r<>0 then failwith "Expecto.Tests.CSharp.exe failed"
    if EnvironmentHelper.isWindows then
        run id ("Expecto.Tests.CSharp/bin/"+configuration+"/netcoreapp1.1/Expecto.Tests.CSharp.dll --summary")

    run id ("Expecto.Focused.Tests/bin/"+configuration+"/netcoreapp2.0/Expecto.Focused.Tests.dll --summary")
    Shell.Exec ("Expecto.Focused.Tests/bin/"+configuration+"/net461/Expecto.Focused.Tests.exe","--summary")
    |> fun r -> if r<>0 then failwith "Expecto.Focused.Tests.exe failed"
    if EnvironmentHelper.isWindows then
        run id ("Expecto.Focused.Tests/bin/"+configuration+"/netcoreapp1.1/Expecto.Focused.Tests.dll --summary")
)

Target "Pack" (fun _ ->
    let packParameters name =
        [
            "--no-build"
            "--no-restore"
            sprintf "/p:Title=\"%s\"" name
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
    DotNetCli.RunCommand id
        ("pack Expecto/Expecto.fsproj -c "+configuration + " -o ../bin " + (packParameters "Expecto"))
    DotNetCli.RunCommand id
        ("pack Expecto.FsCheck/Expecto.FsCheck.fsproj -c "+configuration + " -o ../bin " + (packParameters "Expecto.FsCheck"))
    DotNetCli.RunCommand id
        ("pack Expecto.BenchmarkDotNet/Expecto.BenchmarkDotNet.fsproj -c "+configuration + " -o ../bin " + (packParameters "Expecto.BenchmarkDotNet"))
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