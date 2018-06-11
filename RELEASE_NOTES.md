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
