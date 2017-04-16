#I @"packages/build/FAKE/tools/"
#r @"FakeLib.dll"
System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__
open System
open Fake
open Fake.Testing.Expecto

// Pattern specifying assemblies to be tested using expecto
let testExecutables = "./Expecto.Tests/**/bin/Release/*Tests*.exe"
let testFromTestDataExecutables = "./testdata/**/bin/Release/*Tests*.exe"

let run cmd args dir =
  if execProcess( fun info ->
    info.FileName <- cmd
    if not( String.IsNullOrWhiteSpace dir) then
      info.WorkingDirectory <- dir
    info.Arguments <- args
  ) System.TimeSpan.MaxValue = false then
      failwithf "Error while running '%s' with args: %s" cmd args


let project = "Expecto"
let summary = "A smooth unit test framework for F#"
let description = summary

// List of author names (for NuGet package)
let authors = [ "Logibit AB" ]
let copyright = "(c) 2016 by Henrik Feldt, formerly Fuchu by @mausch"
// Tags for your project (for NuGet package)

let version = { Major = 5; Minor = 0; Patch = 0 }

open Fake.AssemblyInfoFile

let genFSAssemblyInfo (projectPath) =
    let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
    let folderName = System.IO.Path.GetFileName(System.IO.Path.GetDirectoryName(projectPath))
    let basePath = folderName
    let fileName = basePath @@ "AssemblyVersionInfo.fs"
    CreateFSharpAssemblyInfo fileName [ 
        Attribute.Title (projectName)
        Attribute.Product project
        Attribute.Copyright copyright
        Attribute.Company (authors |> String.concat ", ")
        Attribute.Description summary
        Attribute.Version <| string version
        // Attribute.Version release.AssemblyVersion
        // Attribute.FileVersion release.AssemblyVersion
        // Attribute.InformationalVersion release.NugetVersion 
    ]


// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let fsProjs =  !! "./**/*.fsproj" |> Seq.filter (fun s -> not <| s.Contains "preview")
    fsProjs |> Seq.iter genFSAssemblyInfo
)


// --------------------------------------------------------------------------------------
// Rename Logary Facades for Expecto
//  (why this is necessary - https://github.com/logary/logary/#using-logary-in-a-library)

Target "ExpectoChangeo" (fun _ ->
    ReplaceInFiles [
        "namespace Logary.Facade", "namespace Expecto.Logging"
        "namespace Logary.Facade.Messages", "namespace Expecto.Logging.Messages"
        "open Logary.Facade", "open Expecto.Logging"
        "open Logary.Facade.Messages", "open Expecto.Logging.Messages"
    ][
        "paket-files/logary/logary/src/Logary.Facade/Facade.fs"
    ]
)


// --------------------------------------------------------------------------------------
// Clean Build Detritus

Target "Clean" (fun _ ->
  !! "/**/bin/"
  ++ "/**/obj/"
  |> CleanDirs
  |> ignore
)


// --------------------------------------------------------------------------------------
// Build library, test project, & sample

let solutionFile = "Expecto"
MSBuildDefaults <- { MSBuildDefaults with Verbosity = Some Minimal }

Target "Build" (fun _ ->
  !! (solutionFile + ".sln")
  |> MSBuildWithDefaults "Rebuild"
  |> ignore
)

Target "BuildFast" (fun _ ->
  !! (solutionFile + ".sln")
  |> MSBuildWithDefaults "Build"
  |> ignore
)

Target "RunTests" (fun _ ->
    !! testExecutables ++ testFromTestDataExecutables
    |> Expecto id
    |> ignore
)

// --------------------------------------------------------------------------------------
// Build netcore expecto library

let netcoreDir = "Expecto.netcore"
let netcoreTestsDir = "Expecto.netcore.Tests"
let dotnet args dir = run "dotnet" args dir

Target "DotnetBuild" (fun _ ->
  dotnet "--info"  ""
  dotnet "restore" netcoreDir
  dotnet "build"   netcoreDir
)

Target "DotnetRunTests" (fun _ ->
  dotnet "--info"  ""
  dotnet "restore" netcoreTestsDir
  dotnet "run -c Release --parallel --fail-on-focused-tests --summary --version" netcoreTestsDir
)

Target "CreateNuGets" (fun _ ->
  let result =
    ExecProcess (fun info ->
      info.FileName <- "cmd"
      info.Arguments <- "/c bundle exec rake create_nugets"
    ) (TimeSpan.FromMinutes 5.0)
  if result <> 0 then failwithf "NuGet fail"
)

// --------------------------------------------------------------------------------------
// Target Dependencies

Target "All" DoNothing


"AssemblyInfo"
  ==> "ExpectoChangeo"
  ==> "Clean"
  ==> "Build"
  ==> "DotnetRunTests"
  ==> "RunTests"
  <=> "DotnetBuild"

"Build"       ==> "All"
"DotnetBuild" ==> "All"

RunTargetOrDefault "DotnetBuild"
