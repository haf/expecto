#!/bin/bash
mono .paket/paket.exe restore
mono packages/build/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx