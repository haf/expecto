using System;
using System.Threading.Tasks;
using Expecto.CSharp;
using Expecto;
using static Expecto.Impl;

namespace Test.CSharp
{
    public class CSharpPrinter : ITestPrinter
    {
        Task ITestPrinter.BeforeEach(string value)
        {
            return Task.CompletedTask;
        }

        Task ITestPrinter.BeforeRun(Expecto.Test value)
        {
            return Task.CompletedTask;
        }

        Task ITestPrinter.Exn(string value1, Exception value2, TimeSpan value3)
        {
            return Task.CompletedTask;
        }

        Task ITestPrinter.Failed(string value1, string value2, TimeSpan value3)
        {
            return Task.CompletedTask;
        }

        Task ITestPrinter.Ignored(string value1, string value2)
        {
            return Task.CompletedTask;
        }

        Task ITestPrinter.Info(string value)
        {
            return Task.CompletedTask;
        }

        Task ITestPrinter.Passed(string value1, TimeSpan value2)
        {
            return Task.CompletedTask;
        }

        Task ITestPrinter.Summary(ExpectoConfig value1, TestRunSummary value2)
        {
            return Task.CompletedTask;
        }
    }

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
                Runner.TestCase("async Action", async () => {
                    await Task.Delay(100);
                    Console.Write("task");
                }),
                Runner.PendingTestCase("pending async Action", async () => {
                    await Task.Delay(100);
                    Console.Write("task");
                }),
                Runner.FocusedTestCase("focused async Action", async () => {
                    await Task.Delay(100);
                    Console.Write("task");
                })
            });

        public static int Main(string[] argv) => Runner.RunTestsInAssembly(Runner.DefaultConfig.WithMySpiritIsWeak(false).AddPrinter(new CSharpPrinter()), argv);
    }
}
