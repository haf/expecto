using System;
using System.Threading.Tasks;
using Expecto.CSharp;
using Expecto;

namespace Test.CSharp
{
    public class Samples
    {
        [Tests]
        public static Expecto.Test blah =
            Runner.TestList("general groupings", new Expecto.Test[] {
                Runner.TestCase("standard", () => Console.Write("standard")),
                Runner.TestCase("standard async", async () => {
                    await Task.Delay(100);
                    Console.Write("standard task");
                }),
                Runner.PendingTestCase("pending", () => Console.Write("pending")),
                Runner.PendingTestCase("pending async", async () => {
                    await Task.Delay(100);
                    Console.Write("standard task");
                }),
                Runner.FocusedTestCase("focused", () => Console.Write("focused")),
                Runner.FocusedTestCase("focused async", async () => {
                    await Task.Delay(100);
                    Console.Write("standard task");
                }),
            });

		public static int Main(string[] argv) => Runner.RunTestsInAssembly(Runner.DefaultConfig, argv);
    }
}
