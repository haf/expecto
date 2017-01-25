using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Expecto;
using static Expecto.Impl;
using Microsoft.FSharp.Core;
using Microsoft.FSharp.Collections;

namespace FSharpConverter
{
    public static class ToFSharpFuncConverterExtensions
    {
        private static readonly Unit Unit = (Unit)Activator.CreateInstance(typeof(Unit), true);
        public static Func<Unit, Unit> ToFunc(this Action action) => x => {action(); return Unit; };
        public static Func<T, Unit> ToFunc<T>(this Action<T> action) => x => { action(x); return Unit; };
        public static FSharpFunc<T, Unit> ToFSharpFunc<T>(this Action<T> action) => FSharpFunc<T, Unit>.FromConverter(new Converter<T, Unit>(action.ToFunc()));
        public static FSharpFunc<Unit, Unit> ToFSharpFunc(this Action action) => FSharpFunc<Unit, Unit>.FromConverter(new Converter<Unit, Unit>(action.ToFunc()));
    }
}

namespace Expecto
{
    using FSharpConverter;

    public static class CSharp
    {
        public static int RunTests(ExpectoConfig config, Test tests) => runEval(config, tests);
        public static int RunTestsWithArgs(ExpectoConfig config, string[] args, Test tests) => Tests.runTestsWithArgs(config, args, tests);
        public static int RunTestsInAssembly(ExpectoConfig config, string[] args) => Tests.runTestsInAssembly(config, args);
        public static void ListTests(Test tests) => Tests.listTests(tests);
        public static Test TestList(string name, IEnumerable<Test> tests) => Tests.testList(name, ListModule.OfSeq(tests));
        public static Test TestCase(string name, Action test) => Tests.testCase(name, test.ToFSharpFunc());
        public static Test TestCase(string name, Task test) => Tests.testCaseAsync(name, Async.awaitTask(test));
        public static Test PendingTestCase(string name, Action test) => Tests.ptestCase(name, test.ToFSharpFunc());
        public static Test PendingTestCase(string name, Task test) => Tests.ptestCaseAsync(name, Async.awaitTask(test));
        public static Test FocusedTestCase(string name, Action test) => Tests.ftestCase(name, test.ToFSharpFunc());
        public static Test FocusedTestCase(string name, Task test) => Tests.ftestCaseAsync(name, Async.awaitTask(test));
        public static ExpectoConfig DefaultConfig => Tests.defaultConfig;
    }
}
