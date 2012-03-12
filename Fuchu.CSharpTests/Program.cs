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
                var setupMemoryStream =
                    Test.Setup<MemoryStream>(setup: () => new MemoryStream(),
                                             teardown: s => { s.Dispose(); });

                yield return 
                    Test.List("Setup & teardown with memorystream", new[] {
                        Test.Case("Can read", setupMemoryStream(ms => {
                            Assert.True(ms.CanRead);
                        })),
                        Test.Case("Can write", setupMemoryStream(ms => {
                            Assert.True(ms.CanWrite);
                        })),
                    });
            }
        }

        private static int Main(string[] args) {
            return Tests
                .Select(t => t.Wrap(x => x.Timeout(2500))) // set a timeout for all tests
                .List()
                .RunParallel(); // run all tests in parallel
        }
    }
}