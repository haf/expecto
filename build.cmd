@echo off

.paket\paket.bootstrapper.exe 4.1.7
if errorlevel 1 (
  exit /b %errorlevel%
)


.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*