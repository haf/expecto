using Fuchu;
using NUnit.Framework;

namespace Fuchu.CSharpTests {
    internal class Program {
        public static void Add() {
            Assert.AreEqual(5, 2+2);
        }

        private static int Main(string[] args) {
            return Test.NewList(Add).RunParallel();
        }
    }
}