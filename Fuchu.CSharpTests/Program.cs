using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using NUnit.Framework;

namespace Fuchu.CSharpTests {
    internal class Program {
        public static IEnumerable<Test> Tests {
            get {
                // simplest test
                yield return 
                    Test.Case("Basic test", () => Assert.AreEqual(4, 2 + 2));

                // setup/teardown
                var withMemoryStream =
                    Test.Setup<MemoryStream>(setup: () => new MemoryStream(),
                                             teardown: s => { s.Dispose(); });

                yield return 
                    Test.List("Setup & teardown with memorystream", new[] {
                        Test.Case("Can read", withMemoryStream(ms => {
                            Assert.True(ms.CanRead);
                        })),
                        Test.Case("Can write", withMemoryStream(ms => {
                            Assert.True(ms.CanWrite);
                        })),
                    });

                var withTempFile =
                    Test.Setup<string>(setup: Path.GetTempFileName,
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
                                Assert.AreEqual(msg.Length, fileLength);
                            }
                        }));
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