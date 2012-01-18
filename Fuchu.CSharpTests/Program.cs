using System.Collections.Generic;
using System.IO;
using NUnit.Framework;

namespace Fuchu.CSharpTests {
    internal class Program {
        public static IEnumerable<Test> Tests() {
            yield return Test.Case("Basic test", () => Assert.AreEqual(4, 2 + 2));

            var setupMemoryStream =
                Test.Setup<MemoryStream>(setup: () => new MemoryStream(),
                           teardown: s => {
                               Assert.AreEqual(5, s.Capacity);
                               s.Dispose();
                           });
            yield return Test.List("setup & teardown",
                                      Test.Case("1", setupMemoryStream(s => s.Capacity = 5))
                                      , Test.Case("2", setupMemoryStream(s => s.Capacity = 5)));
        }

        public static readonly Test MainTest = Test.List("", Tests);

        private static int Main(string[] args) {
            return MainTest.Run();
        }
    }
}