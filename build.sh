#!/bin/bash
mono $PWD/.paket/paket.exe restore
mono $PWD/packages/build/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx