using System.Collections.Generic;
using System.IO;
using Microsoft.FSharp.Collections;
using NUnit.Framework;

namespace Fuchu.CSharpTests {
    internal class Program {
        public static IEnumerable<Test> Tests() {
            yield return Test.NewCase("Basic test", () => Assert.AreEqual(4, 2 + 2));

            var setupMemoryStream =
                Test.Setup<MemoryStream>(setup: () => new MemoryStream(),
                           teardown: s => {
                               Assert.AreEqual(5, s.Capacity);
                               s.Dispose();
                           });
            yield return Test.NewList("setup & teardown",
                                      Test.NewCase("1", setupMemoryStream(s => s.Capacity = 5))
                                      , Test.NewCase("2", setupMemoryStream(s => s.Capacity = 5)));
        }

        public static readonly Test MainTest = Test.NewList("", Tests);

        private static int Main(string[] args) {
            return MainTest.Run();
        }
    }
}