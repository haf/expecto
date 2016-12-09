@echo off

.paket\paket.bootstrapper.exe 4.0.0-alpha030 --max-file-age=120
if errorlevel 1 (
  exit /b %errorlevel%
)


.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*