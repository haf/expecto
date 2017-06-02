#r "./packages/build/FAKE/tools/FakeLib.dll"
#r "./packages/build/FSharp.Configuration/lib/net46/FSharp.Configuration.dll"

open Fake
open Fake.AssemblyInfoFile
open Fake.Git
open Fake.Testing
open FSharp.Configuration
open System
open System.IO

type SemVer = YamlConfig<".semver">

[<Literal>]
let AppName = "Expecto"

[<Literal>]
let Author = "haf"

let buildVersion =
    let semverData = SemVer()
    semverData.Load ".semver"
    let patch =
        semverData.``:patch``
        |> string
        |> environVarOrDefault "APPVEYOR_BUILD_NUMBER"
    let special =
        match semverData.``:special`` with
            | "" -> ""
            | x -> sprintf "-%s" x
    sprintf "%d.%d.%s%s" semverData.``:major`` semverData.``:minor`` patch special

[<Literal>]
let BuildDir = "build/pkg/"

// Filesets
let codeProjects = !!"./src/**/*.fsproj"
let testProjects = !!"./tests/**.*proj"
let testAssemblies = !!"./test/**/net461/*.exe"

let isOriginalAuthor =
    match buildServer with
    | AppVeyor -> environVar "APPVEYOR_REPO_COMMIT_AUTHOR" = Author
    | _ -> true

let attributes = 
    let buildDate = DateTime.UtcNow.ToString()
    [ Attribute.Title AppName
      Attribute.Description "A smooth unit test framework for F#"
      Attribute.Company "Logibit AB"
      Attribute.Copyright
          "(c) 2016 by Henrik Feldt, formerly Fuchu by @mausch"
      Attribute.Metadata("Build Date", buildDate)
      
      Attribute.Version buildVersion
      Attribute.FileVersion buildVersion
      Attribute.InformationalVersion buildVersion]

type ReleaseBuildType = | DoNotRelease | TagOnly | FullRelease

let isReleaseCommit =
    // AppVeyor will push a tag first, and then the build for the tag will publish to NuGet.org
    let bumpsVersion =
        CommitMessage.getCommitMessage currentDirectory
        |> toLower
        |> startsWith "bump version"
    match buildServer with
    | AppVeyor -> bumpsVersion && isOriginalAuthor
    | LocalBuild ->
        if bumpsVersion then
            tracefn "Running from local build; it is assumed that a GitHub release is intended..."
        bumpsVersion
    | x -> failwithf "Unknown build environment: %A" x

let packFunc proj (x: DotNetCli.PackParams) =
    {x with
        Project = proj
        Configuration = "Release"
        OutputPath = Directory.GetCurrentDirectory() @@ BuildDir
        AdditionalArgs =
            [
                "--no-build"
                sprintf "/p:Version=%s" buildVersion
            ]}

let pushFunc url apiEnv (x: Paket.PaketPushParams) =
    {x with
        ApiKey = environVarOrFail apiEnv
        PublishUrl = url
        WorkingDir = BuildDir}

// Targets
Target "Clean" (fun _ -> DotNetCli.RunCommand id "clean")

Target "CleanBuildOutput" (fun _ -> DeleteDir BuildDir)

Target "FixLogary"
    (fun _ ->
        let filename = "paket-files/logary/logary/src/Logary.Facade/Facade.fs"
        let mutable content = File.ReadAllText filename
        content <- content.Replace("namespace Logary.Facade", "namespace Expecto.Logging")
        File.WriteAllText(filename, content))

Target "AssemblyInfo"
    (fun _ -> 
        CreateFSharpAssemblyInfo "AssemblyVersionInfo.fs" attributes
        CreateCSharpAssemblyInfo "AssemblyVersionInfo.cs" attributes)

Target "Restore" (fun _ -> DotNetCli.Restore id)

Target "Build" (fun _ -> DotNetCli.Build (fun p -> {p with Configuration = "Release"}))

Target "Pack" (fun _ -> codeProjects |> Seq.iter (packFunc >> DotNetCli.Pack))

Target "Test" (fun _ -> Expecto.Expecto id testAssemblies)

Target "CheckPendingChanges"
    (fun _ ->
        if not <| Git.Information.isCleanWorkingCopy currentDirectory then
            failwith "Repository is not clean."
        else
            printfn "Repository is clean.")

Target "PushToNuGet" (fun _ -> Paket.Push (pushFunc "https://api.nuget.org/v3/index.json" "nuget_key"))

Target "GitTag"
    (fun _ ->
        let tagName = sprintf "v%s" buildVersion
        Branches.tag currentDirectory tagName
        Branches.pushTag currentDirectory "origin" tagName)

// Build order
"CleanBuildOutput"
    ==> "Clean"
    ==> "AssemblyInfo"
    ==> "Restore"
    ==> "FixLogary"
    ==> "Build"
    ==> "Pack"
    ==> "Test"
    ==> "CheckPendingChanges"
    =?> ("GitTag", isReleaseCommit)
    =?> ("PushToNuGet", isReleaseCommit)
"Build" ==> "Test"
// start build
RunTargetOrDefault "Pack"