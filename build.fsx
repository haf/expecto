#I @"packages/FAKE/tools/"
#r @"FakeLib.dll"
#r "System.Xml.Linq"
#r ".nuget/NuGet.exe"

open System
open Fake
open Fake.ProcessHelper
open Fake.AssemblyInfoFile

let version = "0.5.0.0"
let assemblyVersion = "0.5.0.0"

type Package = {
    Name: string
    Author: string
    Description: string
    Sign: bool
    Dependencies: seq<string * string>
}

module Pkg =
    open System
    open System.IO
    open System.Xml.Linq
    open global.NuGet

    let dependenciesFromConfig (path: string) = 
        XDocument.Load(path @@ "packages.config").Root.Elements(XName.Get "package")
        |> Seq.map (fun e -> e.Attribute(XName.Get "id").Value, e.Attribute(XName.Get "version").Value)
        |> Seq.toList

    let build (p: Package) =
        let builder = 
            PackageBuilder(
                Id = p.Name,
                Title = p.Name,
                Version = SemanticVersion version,
                Description = p.Description,
                LicenseUrl = Uri("https://raw.githubusercontent.com/mausch/Fuchu/master/license.txt"),
                Language = "en-US",
                ProjectUrl = Uri("https://github.com/mausch/Fuchu")
            )
        builder.Authors.Add p.Author |> ignore
        builder.Owners.Add "Mauricio Scheffer" |> ignore
        let packageFiles =
            ["pdb"; "xml"; "dll"]
            |> Seq.collect (fun e -> !! (p.Name @@ "bin" @@ "Release" @@ (p.Name + "." + e)))
            |> Seq.map (fun f -> PhysicalPackageFile(SourcePath = f, TargetPath = "lib" @@ Path.GetFileName f))
        builder.Files.AddRange (packageFiles |> Seq.map (fun i -> upcast i))
        if builder.Files.Count = 0 then failwithf "No files defined for package %s" p.Name
        let deps = 
            p.Dependencies
            |> Seq.map (fun (id,v) -> PackageDependency(id = id, versionSpec = VersionUtility.ParseVersionSpec v))
        builder.DependencySets.Add(PackageDependencySet(null, deps))
        use fs = File.Create (sprintf "%s.%s%s" p.Name version Constants.PackageExtension)
        builder.Save fs

let packages = 
    [
        { Package.Name = "Fuchu"
          Author = "Mauricio Scheffer"
          Description = "Functional test library"
          Sign = true
          Dependencies = [] }

        { Package.Name = "Fuchu.MbUnit"
          Author = "Mauricio Scheffer"
          Description = "Converts Fuchu tests to MbUnit tests"
          Sign = true
          Dependencies = ("Fuchu", version)::(Pkg.dependenciesFromConfig "Fuchu.MbUnit") }

        { Package.Name = "Fuchu.FsCheck"
          Author = "Mauricio Scheffer"
          Description = "Integrates Fuchu with FsCheck"
          Sign = false
          Dependencies = ("Fuchu", version)::(Pkg.dependenciesFromConfig "Fuchu.FsCheck") }

        { Package.Name = "Fuchu.PerfUtil"
          Author = "Henrik Feldt"
          Description = "Integrates Fuchu with PerfUtil"
          Sign = false
          Dependencies = ("Fuchu", version)::(Pkg.dependenciesFromConfig "Fuchu.PerfUtil") }
    ]

Target "BuildSolution" (fun _ ->
    MSBuildRelease null "Rebuild" ["./Fuchu.sln"]
    |> Log "AppBuild-Output: "
)

Target "NuGet" <| fun _ ->
    List.iter Pkg.build packages


Target "AssemblyInfo" <| fun _ ->
    let asmInfo (p: Package) =
        let attributes = [
            Attribute.Version assemblyVersion
            Attribute.FileVersion version
            Attribute.Title p.Name
            Attribute.Product p.Name
            Attribute.Description p.Description
            Attribute.Copyright (sprintf "Copyright %s %d" p.Author DateTime.Now.Year)
        ]
        let attributes = 
            if p.Sign 
                then (Attribute.KeyFile "../Fuchu.snk")::attributes
                else attributes
        CreateFSharpAssemblyInfo (p.Name @@ "AssemblyInfo.fs") attributes
    List.iter asmInfo packages

Target "Test" <| fun _ ->
    let errorCode = 
        [
            "Fuchu.Tests"
            "Fuchu.CSharpTests"
        ]
        |> Seq.map (fun t -> t @@ "bin" @@ "Release" @@ (t + ".exe"))
        |> Seq.map (fun p -> if not isMono then p,null else "mono",p)
        |> Seq.map (fun (p,a) -> asyncShellExec { defaultParams with Program = p; CommandLine = a })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.sum
    if errorCode <> 0 then failwith "Error in tests"

"BuildSolution" <== ["AssemblyInfo"]
"Test" <== ["BuildSolution"]
"NuGet" <== ["Test"]

RunTargetOrDefault "NuGet"
