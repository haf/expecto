using System;
using System.Threading.Tasks;
using static Expecto.CSharp.Runner;
using Expecto;

namespace Test.CSharp
{
    public class Samples
    {
        [Tests]
        public static Expecto.Test blah =
            TestList("general groupings", new Expecto.Test[] {
                TestCase("standard", () => Console.Write("standard")),
                TestCase("standard async", async () => {
                    await Task.Delay(100);
                    Console.Write("standard task");
                }),
                PendingTestCase("pending", () => Console.Write("pending")),
                PendingTestCase("pending async", async () => {
                    await Task.Delay(100);
                    Console.Write("standard task");
                }),
                FocusedTestCase("focused", () => Console.Write("focused")),
                FocusedTestCase("focused async", async () => {
                    await Task.Delay(100);
                    Console.Write("standard task");
                }),
            });

		public static int Main(string[] argv) => RunTestsInAssembly(DefaultConfig, argv);
    }
}
