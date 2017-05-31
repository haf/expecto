@echo off

.paket\paket.bootstrapper.exe 5.0.0-beta008
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe restore group Build
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*
