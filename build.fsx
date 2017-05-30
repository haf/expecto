#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.AppVeyor
open Fake.AssemblyInfoFile
open Fake.Git
open System
open System.IO

[<Literal>]
let AppName = "Expecto"

let buildVersion = File.ReadAllText ".version"

[<Literal>]
let BuildDir = "pkg/"

// Filesets
let projectFiles = !!"/**/*.fsproj" ++ "/**.*.csproj"
let testProjects, codeProjects = projectFiles |> List.ofSeq |> List.partition (fun x -> x.Contains("test"))

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

Target "Build" (fun _ -> DotNetCli.Build id)

Target "Pack" (fun _ -> codeProjects
                           |> List.iter
                            (fun x -> DotNetCli.Pack (fun p -> {p with  Project = x;
                                                                        Configuration = "Release";
                                                                        OutputPath = sprintf "./../%s" BuildDir;})))

Target "Test" (fun _ -> testProjects
                           |> List.iter
                            (fun x -> DotNetCli.RunCommand (fun p -> {p with WorkingDir = DirectoryName x}) "run -f netcoreapp1.1" ))

// Build order
"CleanBuildOutput" ==> "Clean"
"CleanBuildOutput"
    ==> "ApplyVersion"
    ==> "AssemblyInfo"
    ==> "FixLogary"
    ==> "Restore"
    // ==> "Build" // Test and Pack do the building.
    ==> "Test"
    ==> "Pack"
// start build
RunTargetOrDefault "Pack"