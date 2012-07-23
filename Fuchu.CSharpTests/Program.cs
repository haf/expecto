using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Fuchu;

namespace FuchuCSharpTests {
    internal class Program {
        public static IEnumerable<Test> Tests {
            get {
                // simplest test
                yield return 
                    Test.Case("Basic test", () => Assert.Equal("2+2", 4, 2 + 2));

                Func<Action<MemoryStream>, Action> withMemoryStream =
                    f => () => {
                        using (var ms = new MemoryStream())
                            f(ms);
                    };
                
                yield return 
                    Test.List("Setup & teardown with memorystream", withMemoryStream, new[] {
                        Test.Case("Can read", (MemoryStream ms) => {
                            Assert.Equal("can read", true, ms.CanRead);
                        }),
                        Test.Case("Can write", (MemoryStream ms) => {
                            Assert.Equal("can write", true, ms.CanWrite);
                        }),
                    });

                var withTempFile =
                    Test.Fixture<string>(setup: Path.GetTempFileName,
                                       teardown: File.Delete);

                // composing setups/teardowns
                Func<Action<MemoryStream, string>, Action> withMemoryStreamAndTempFile =
                    f => withMemoryStream(ms => withTempFile(s => f(ms, s))());

                yield return
                    Test.Case("Composed setups", 
                        withMemoryStreamAndTempFile((ms, filename) => {
                            const string msg = "Hello World!";
                            using (var tw = new StreamWriter(ms)) {
                                tw.Write(msg);
                                tw.Flush();
                                ms.Position = 0;
                                File.WriteAllBytes(filename, ms.ToArray());
                                var fileLength = new FileInfo(filename).Length;
                                Assert.Equal("file length", msg.Length, fileLength);
                            }
                        }));

                yield return
                    Test.Case("skipped test", () => 
                        Test.Skip("I just wanted to {0} this {1}", "skip", "test"));

                //yield return Test.Case("failed test", () => Test.Fail(""));

                // FsCheck integration
                yield return Fuchu.FsCheck.Property("Addition is commutative",
                                                        (int a, int b) => a + b == b + a);
            }
        }

        private static int Main(string[] args) {
            return Tests
                .Select(t => t.Wrap(x => x.Timeout(2500))) // set a timeout for each test
                .List()
                .Run(); //.RunParallel(); or run all tests in parallel
        }
    }
}