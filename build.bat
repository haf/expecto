@echo off

cls
if %PROCESSOR_ARCHITECTURE%==x86 (
         set MSBUILD="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe"
) else ( set MSBUILD="%SystemRoot%\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"
)
if not exist .nuget\nuget.exe %MSBUILD% .nuget\nuget.targets /t:CheckPrerequisites
if not exist packages\FAKE\tools\Fake.exe ( 
	echo Downloading FAKE...
	".nuget\NuGet.exe" "install" "FAKE" "-OutputDirectory" "packages" "-ExcludeVersion" "-Prerelease"
)
"packages\FAKE\tools\Fake.exe" "build.fsx" %*
