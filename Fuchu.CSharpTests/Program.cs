using Fuchu;
using Microsoft.FSharp.Core;

namespace Fuchu.CSharpTests {
    internal class Program {
        public static FSharpChoice<Unit,string> Add() {
            return F.AssertEqual(4, 2 + 2);
        }

        private static int Main(string[] args) {
            return Test.NewList(Add).RunParallel();
        }
    }
}