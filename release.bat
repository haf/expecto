msbuild /m /p:Configuration=Release fuchu.sln
.nuget\bin\nuget pack Fuchu.nuspec
.nuget\bin\nuget pack Fuchu.FsCheck.nuspec