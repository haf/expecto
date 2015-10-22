#!/bin/bash
set -e
NUGET=./.nuget/NuGet.exe
if [ ! -f $NUGET ]; then
  curl -sSL http://nuget.org/nuget.exe -o nuget.exe
  mv nuget.exe $NUGET 
  chmod a+x $NUGET
fi
FAKE=packages/FAKE/tools/FAKE.exe
if [ ! -f $FAKE ]; then
  echo Downloading FAKE...
  mono $NUGET install FAKE -OutputDirectory packages -ExcludeVersion -Prerelease
fi
mono $NUGET restore
mono $FAKE build.fsx "$@"
