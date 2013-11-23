NUGET=./.nuget/NuGet.exe
if [ ! -f $NUGET ]; then
  wget http://nuget.org/nuget.exe
  mv nuget.exe $NUGET 
  chmod a+x $NUGET
fi
FAKE=packages/FAKE/tools/FAKE.exe
if [ ! -f $FAKE ]; then
  echo Downloading FAKE...
  mono $NUGET install FAKE -OutputDirectory packages -ExcludeVersion -Prerelease
fi
mono $FAKE build.fsx "$@"
