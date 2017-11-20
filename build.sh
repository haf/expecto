#!/bin/bash
ls
chmod u+x .paket/paket.exe
mono $PWD/.paket/paket.exe restore
mono $PWD/packages/build/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx