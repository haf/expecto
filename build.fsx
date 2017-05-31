#r "./packages/build/FAKE/tools/FakeLib.dll"
#r "./packages/build/FSharp.Configuration/lib/net46/FSharp.Configuration.dll"

open Fake
open Fake.AssemblyInfoFile
open Fake.Testing
open FSharp.Configuration
open System
open System.IO

type SemVer = YamlConfig<".semver">

[<Literal>]
let AppName = "Expecto"

let buildVersion =
    let semverData = SemVer()
    semverData.Load ".semver"
    let special =
        match semverData.``:special`` with
            | "" -> ""
            | x -> sprintf ".%s" x
    sprintf "%d.%d.%d%s" semverData.``:major`` semverData.``:minor`` semverData.``:patch`` special

[<Literal>]
let BuildDir = "build/pkg/"

// Filesets
let projectFiles = !!"/**/*.fsproj" ++ "/**.*.csproj"
let testProjects, codeProjects = projectFiles |> List.ofSeq |> List.partition (fun x -> x.ToLower().Contains("test"))
let testAssemblies =
    !!"/**/*.exe"
    |> Seq.filter
        (fun x ->
            let x = x.ToLower()
            x.Contains("test") && x.Contains("bin"))

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

// Targets
Target "Clean" (fun _ -> DotNetCli.RunCommand id "clean")

Target "CleanBuildOutput" (fun _ -> DeleteDir BuildDir)

Target "ApplyVersion" (fun _ -> 
        let mutable content = File.ReadAllText "./tools/version.props.template"
        content <- content.Replace("@Version", buildVersion)
        File.WriteAllText("./tools/version.props", content))

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

Target "Pack" (fun _ -> codeProjects
                           |> List.filter (fun x -> not <| x.Contains("Sample"))
                           |> List.iter
                            (fun x -> DotNetCli.Pack (fun p -> {p with  Project = x;
                                                                        Configuration = "Release";
                                                                        AdditionalArgs = ["--no-build"]
                                                                        OutputPath = sprintf "./../%s" BuildDir;})))

Target "Test" (fun _ -> Expecto.Expecto id testAssemblies)

// Build order
"ApplyVersion"
    ==> "Clean"
    ==> "AssemblyInfo"
    ==> "FixLogary"
    ==> "Restore"
    ==> "Build"
    ==> "CleanBuildOutput"
    ==> "Pack"
"Build" ==> "Test"
// start build
RunTargetOrDefault "Pack"