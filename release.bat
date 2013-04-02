msbuild /m /p:Configuration=Release fuchu.sln
.nuget\nuget.exe pack Fuchu.nuspec
.nuget\nuget.exe pack Fuchu.FsCheck.nuspec
.nuget\nuget.exe pack Fuchu.MbUnit.nuspec