### 11.0.0-alpha1 - 2024-08-14
* Fix testTheory issue where null and empty string produce duplicate test names (#494), thanks @Numpsy
* Breaking Change: FsCheck 3 is now the default for Expecto.FsCheck, since FsCheck 2 is no longer supported. FsCheck 2 support is still available under the `-fscheck2` version suffix (i.e. install Expecto.FsCheck with version [11.0.0-alpha1-fscheck2](https://www.nuget.org/packages/Expecto.FsCheck/11.0.0-alpha1-fscheck2))
* Breaking Change: move `FsCheckConfig.replay` from `int` to `uint64` (#501), thanks @rynoV
  * Fixes issue where many FsCheck3 runs could not be replayed since the random seed is too large.
  * Existing FsCheck 2 users should be able to use the same seeds values, but converted to `uint64`.
  * `uint64` literals can be defined like `let iAm5 = 5UL`

### 10.2.1 - 2024-03-15
* Fix bug where testTask and testCaseTask allow the tasks to start immediately when the test is defined, breaking backward compatibility with testTask.

### 10.2.0 - 2024-03-15
* Add throwsAsync, throwsAsyncT, and throwsAsyncC (#469), thanks @jwosty
* Add Nullable<'t> helpers to Expect module (#478), thanks @bisen2
* Add *testCaseTask functions (#483), thanks @ratsclub and @farlee2121
* Support ValueTask and Async bindings in testTask expressions (#489), thanks @farlee2121

### 10.1.0 - 2023-06-26
* Add Theory Test Support #456, thanks @ratsclub
* Loosen the FSharp.Core version requirement #458, thanks @farlee2121 and @JohnTheGr8
* New Expecto.FsCheck pre-release version with FsCheck3 support #450, thanks @farlee2121
* Add `Flip.Expect.hasLength` #455, thanks @JohnTheGr8

#### 10.0.0 - 2023-04-20
* Make Expect.isNotEmpty accept whitespace strings #452
* Removed deprecated code #449
* Bumped minimum framework to .NET 6 #447
* Add testFixtureAsync and testFixtureTask #444

#### 9.0.3 - 2021-08-17
* Thanks @lydell for improving Expecto.Diff

#### 9.0.3 - 2020-07-27
* List focused tests if failed using `--fail-on-focused-tests`, #392, thanks @vilinski
* Add an option to specify test states in `--list-tests`, thanks @vilinski

#### 9.0.2 - 2020-06-25
* An eta-expansion caused ABI compatibility for dependents, #388, thanks @haf

#### 9.0.1 - 2020-06-24
* Ensure filtered tests are properly filtered #378 #380, thanks @MNie
* Update BenchmarkDotNet to 0.12.1 fixing #381 #384, thanks @MNie
* Add support for while-expressions inside regular test cases #386, thanks @teo-tsirpanis
* Add `Expect.wantError`, which returns the value inside the Result wrapper if successful, thanks @yreynhout

#### 9.0.0 - 2020-04-04
* Change the default test separator to `.` (dot). Override back, using `--join-with /` Big thanks @MNie
* Add `Expect.wantSome` and `Expect.wantOk`, which returns the value inside the Option/Result wrapper if successful, thanks @teo-tsirpanis
* Remove deprecated PackageIconUrl from the build props, replace with PackageIcon, thanks @teo-tsirpanis
* Add cmd file for building on Windows, thanks @teo-tsirpanis
* Port Expecto's own tests to .net core app 3.1, but target netstandard2.0 for the main lib, thanks @teo-tsirpanis
* Rename `parallel` to `runInParallel` in config record to avoid warning, thanks @teo-tsirpanis
* Inline the XML/NUnit/JUnit test result printer in the main assembly. Thanks @teo-tsirpanis
* Split Expecto monofile into separate per surface area, thanks @teo-tsirpanis

#### 8.13.1 - 2019-11-24
* Include Expecto.Diff in build, thanks @haf

#### 8.13.0 - 2019-11-23
* Docs fixes #342 #358, #362, thanks @PhilT @ZaymonFC @dmitrydprog
* Fix TestBuilder bug #341, thanks @haf
* Upgrade Paket, DotNetBenchmark #355, thanks @haf
* Refactor files; split into separate files #353, thanks @haf
* Get inner message exceptions, #349 thanks @MNie
* Update Logary sample, #347 thanks @smoothdeveloper
* LARGE IMPROVEMENT: use DiffPlex for diffs (opt-in right now) #346, thanks @drhumlen
* Configure WhiteSource for security scanning Expecto, #344 thanks @haf
* Report duplicate test names in printer, #339, thanks @auduchinok
* Improve Travis build for .Net Core #338 thanks @auduchinok
* (Missed creds to @TheAngryByrd in 8.12 for improving TestJobBuilder)
* Thank you everyone❣️

#### 8.12.0 - 2019-09-16
* Fix #330 - a possible null ref in coloured logging, thanks @haf

#### 8.11.0 - 2019-07-22
* Update typo in README.md. #323 thanks @MarneeDear
* Several typos. #325 thanks @milbrandt
* Default colours to 8 if not set in config. #330 thanks @AnthonyLloyd
* Aligns TestJobBuilder closer to Hopacs JobBuilder. #333 thanks @TheAngryByrd
* Add Test.shuffle function. thanks @AnthonyLloyd

#### 8.10.1 - 2019-04-07
* Skipping results in an exception report. #313 thanks @AnthonyLloyd
* Progress log does not flush during tests. #314 thanks @AnthonyLloyd

#### 8.10.0 - 2019-04-03
* Add Expecto.hasLength. #312 thanks @drhumlen
* Count when focusing on any test is off. #308 thanks @AnthonyLloyd
* Reported error is unreadable in TeamCity. #309 thanks @AnthonyLloyd
* Add colours switch. #306 thanks @AnthonyLloyd
* Number of other small items. #306 thanks @AnthonyLloyd

#### 8.9.1 - 2019-02-27
* Colors are a bit off bug fix. #305 thanks @AnthonyLloyd

#### 8.9.0 - 2019-02-23
* Args replacement for Argu. #303 thanks @AnthonyLloyd
* New runTests CLI args functions. Please migrate to rather than using ExpectoConfig. thanks @AnthonyLloyd
* Easier to read error output with highlighted differences. #304 thanks @AnthonyLloyd

#### 8.8.0 - 2019-01-28
* Better isFasterThan equal stopping criteria. thanks @AnthonyLloyd
* Remove outliers in isFasterThan statistics. thanks @AnthonyLloyd
* Add atomic printfn shadow function. #267 thanks @AnthonyLloyd

#### 8.7.0 - 2019-01-08
* Upgrade dependencies inc Argu 5.2. #300 thanks @AnthonyLloyd

#### 8.6.5 - 2018-12-20
* fix only getting Expectos stack trace. #298 thanks @AnthonyLloyd
* fix Expecto fails silently on duplicate tests. #299 thanks @AnthonyLloyd

#### 8.6.4 - 2018-12-11
* fix test results xml incomplete. thanks @AnthonyLloyd

#### 8.6.3 - 2018-12-08
* isFasterThan machine resolution fix. thanks @AnthonyLloyd

#### 8.6.2 - 2018-12-08
* isFasterThan machine resolution fix. thanks @AnthonyLloyd

#### 8.6.1 - 2018-12-07
* Tweak colourised console colours, thanks @haf
* Xml summary can cause exception. #296 thanks @AnthonyLloyd
* isFasterThan improve use of warm up runs. thanks @AnthonyLloyd
* taskTask is missing my stack trace. #295 thanks @AnthonyLloyd
* taskTask doesnt surface Task as valid do target. #293 thanks @AnthonyLloyd

#### 8.6.0 - 2018-11-17
* #266 Merge ANSI Output Writer back into Logary Facade, decouple it from Progress, thanks @haf
* #290 Speed up Progress, thanks @haf
* #291 Make ANSI writing the default, so Expecto loggers work outside of framework-provided runXXXX functions, thanks @haf
* Make sure isEqual doesn't crash on null values, thanks @kleidemos
* Using the improved v2 task workflow builder, thanks @AnthonyLloyd

#### 8.5.0 - 2018-11-05
* Add task tests i.e. testTask/ftestTask etc. thanks @AnthonyLloyd
* Add afterRunTests function global teardown. thanks @AnthonyLloyd
* Use Fake.BuildServer and sort tests. thanks @AnthonyLloyd

#### 8.4.3 - 2018-10-20
* Add testLabel. #286 thanks @haf
* Document tests results are nunit v2 and upload. #288 thanks @enricosada
* Fix small progress spinner issues. #285 thanks @AnthonyLloyd
* Make gray really white. #287 thanks @AnthonyLloyd

#### 8.4.2 - 2018-09-28
* Junit report: add option to treat errors like failures. #282 thanks @0x53A
* Add etestProperty for stdgen and keep ftestProperty for just focus. #281 thanks @AnthonyLloyd

#### 8.4.1 - 2018-09-25
* Bug fix for C# IsFasterThan. thanks @AnthonyLloyd

#### 8.4.0 - 2018-09-25
* Create Expecto.TestResults package. thanks @haf
* Create test results directory. #274 thanks @JonCanning
* Add a C# friendly extension method for the nunit testresult xml. #275 thanks @0x53A
* Add JUnit-like summary file. #278 thanks @0x53A
* Expect.equal - locate first different field for records. #280 thanks @MNie
* Expect.stringStarts - underline first difference. thanks @AnthonyLloyd
* Add no-spinner switch. thanks @AnthonyLloyd
* Add C# methods for IsFasterThan. thanks @AnthonyLloyd

#### 8.3.0 – 2018-09-05
* Adding test results file, see #179, thanks @matthid, @haf
* Fix throwsT message on no exception. #272 thanks @otto-gebb

#### 8.2.2 – 2018-08-15
* Store non-atomic printfn output. #267 thanks @t1m0thyj @AnthonyLloyd
* Unwrap AggregateExcpetions with single inner. #269 thanks @0x53A

#### 8.2.1 – 2018-07-30
* Fixed version doesn't seem to work. #268 thanks @AnthonyLloyd
* Add auto flush console output for debug. #267 thanks @AnthonyLloyd
* Attempted fix for progress indicator on iTerm/zsh. #267 thanks @AnthonyLloyd

#### 8.2.0 – 2018-07-13
* Added C# compatible With methods / Logging: add Task based Interface. #259 thanks @0x53A
* Feature/sequencecontainsorder. #262 thanks @MNie
* Console work inc ANSI colours, progress indicator. #258 thanks @AnthonyLloyd
* Upgrade to FAKE 5. #263 thanks @AnthonyLloyd
* Removed inlining. #254 thanks @jackfoxy @AnthonyLloyd

#### 8.1.1 – 2018-06-13
* Compile against 4.3.4 of F# Core for now, thanks @haf

#### 8.1.0 – 2018-06-11
* Logary.Facade corrected to v3 Apache 2.0 version. thanks @haf/@AnthonyLloyd
* Added sourcelink support. #251 thanks @jackfoxy
* Fixed comparison of affine (ephemeral) sequence. #253 thanks @jackfoxy

#### 8.0.0 – 2018-05-08
* Upgrade Logary.Facade and bump major because of a int64 to float change. thanks @haf
* Add docs on how to get extra value/stacktrace parsing when using Expecto. thanks @haf
* Fixed flipped throwsT not passing exception type through. #244 thanks @daniel-chambers
* Fixed all and allEqual call enumeration more than once. #245 thanks @kleidemos
* Add passing cancellation token to run functions. #229 thanks @AnthonyLloyd

#### 7.0.1 - 2018-04-07
* Default printer log with list name. #236 thanks @jackfoxy
* Minor grammar fix. #235 thanks @rmunn
* Update mono.cecil to 0.10.0. #234 thanks @MNie
* foldParallel work fixes ObjectDisposedException. #233 thanks @AnthonyLloyd

#### 7.0.0 - 2018-03-13
* Added Expecto.Hopac. #231 thanks @TheAngryByrd
* Fix Expecto.BenchmarkDotNet TypeLoadException get_ArtifactsPath. #225 thanks @AnthonyLloyd
* Fix Argu 5.1.0 update causes MissingMethodException. #227 thanks @AnthonyLloyd
* Added FSCheck distribution and labels output for failing tests. #221 thanks @AnthonyLloyd
* Major version release due to removal of netstandard1.6 support.

#### 6.0.0 - 2018-03-05
* Allow specifying log name at command line. #224 thanks @rmunn
* Update paket. #222 thanks @MNie
* Increase float display precision. thanks @AnthonyLloyd
* Update build.sh. thanks @haf
* Fix restore errors. #218 thanks @forki

#### 5.1.2 - 2018-01-04
* Update Mono.Cecil to 0.10.0-beta7. #215 thanks @MNie
* Expecto.BenchmarkDotNet .NETCoreApp versions. #214 thanks @AnthonyLloyd

#### 5.1.1 - 2017-12-06
* Framework condition fileversioninfo. #210 thanks @AnthonyLloyd
* Add allow duplicate names config, #211 thanks @AnthonyLloyd
* Add testName function. #199 thanks @AnthonyLloyd

#### 5.1.0 - 2017-12-01
* Changes that are needed to Expecto.BenchmarkDotNet. #196 thanks @MNie
* Feature/duplicated name. #198 thanks @MNie
* Fixed a couple of typos. #200 thanks @JonCanning
* Add Expecto.Flip.Expect module to .NET Standard version. #207 thanks @inosik
* Ported everything to .NET Standard 1.6. #197 thanks @teo-tsirpanis
* Add floatLessThanOrClose and floatGreaterThanOrClose. thanks @AnthonyLloyd
* Add null test during test discovery exception. #202 thanks @AnthonyLloyd
* Add isGroupsMatch and isRegexGroupsMatch. #134 thanks @MNie
* Move to FAKE and support netstandard2.0 and simplify. thanks @AnthonyLloyd
* Add sequenceEqual for items. #206 thanks @haf
* Exceptions lose their stack trace when no line numbers. #205 thanks @AnthonyLloyd
* Update FsCheck and BenchmarkDotNet. thanks @AnthonyLloyd
* Add Performance.findFastest. thanks @AnthonyLloyd
