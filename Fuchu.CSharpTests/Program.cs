using Fuchu;
using Microsoft.FSharp.Core;

namespace Fuch.CSharpTests {
    internal class Program {
        public static FSharpChoice<Unit,string> Add() {
            return F.AssertEqual(4, 2 + 2);
        }

        private static void Main(string[] args) {
            Test.NewList(Add).RunParallel();
        }
    }
}