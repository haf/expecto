#!/bin/bash
export FrameworkPathOverride=$(dirname $(which mono))/../lib/mono/4.6.1-api
mono .paket/paket.exe restore
mono packages/build/FAKE/tools/FAKE.exe $@ --removeLegacyFakeWarning --fsiargs -d:MONO build.fsx
