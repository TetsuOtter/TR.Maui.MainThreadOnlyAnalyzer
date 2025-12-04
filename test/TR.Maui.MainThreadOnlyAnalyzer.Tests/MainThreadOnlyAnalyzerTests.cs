using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Testing;
using Microsoft.CodeAnalysis.Testing;
using Xunit;

namespace TR.Maui.MainThreadOnlyAnalyzer.Tests;

public class MainThreadOnlyAnalyzerTests
{
    private static async Task VerifyAnalyzerAsync(string source, params DiagnosticResult[] expected)
    {
        var test = new CSharpAnalyzerTest<MainThreadOnlyAnalyzer, DefaultVerifier>
        {
            TestCode = source,
            ReferenceAssemblies = ReferenceAssemblies.Net.Net80,
        };

        test.ExpectedDiagnostics.AddRange(expected);
        await test.RunAsync();
    }

    [Fact]
    public async Task NoDiagnostic_WhenMethodCalledFromNormalContext()
    {
        var source = @"
namespace TestNamespace
{
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Property | System.AttributeTargets.Constructor)]
    public sealed class MainThreadOnlyAttribute : System.Attribute { }

    public class TestClass
    {
        [MainThreadOnly]
        public void MainThreadMethod() { }

        public void CallerMethod()
        {
            MainThreadMethod();
        }
    }
}";

        await VerifyAnalyzerAsync(source);
    }

    [Fact]
    public async Task Diagnostic_WhenMethodCalledFromTaskRun()
    {
        var source = @"
using System.Threading.Tasks;

namespace TestNamespace
{
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Property | System.AttributeTargets.Constructor)]
    public sealed class MainThreadOnlyAttribute : System.Attribute { }

    public class TestClass
    {
        [MainThreadOnly]
        public void MainThreadMethod() { }

        public void CallerMethod()
        {
            Task.Run(() =>
            {
                {|#0:MainThreadMethod()|};
            });
        }
    }
}";

        var expected = new DiagnosticResult("MAUIMT001", DiagnosticSeverity.Warning)
            .WithLocation(0)
            .WithArguments("MainThreadMethod");

        await VerifyAnalyzerAsync(source, expected);
    }

    [Fact]
    public async Task Diagnostic_WhenMethodCalledFromThreadPoolQueueUserWorkItem()
    {
        var source = @"
using System.Threading;

namespace TestNamespace
{
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Property | System.AttributeTargets.Constructor)]
    public sealed class MainThreadOnlyAttribute : System.Attribute { }

    public class TestClass
    {
        [MainThreadOnly]
        public void MainThreadMethod() { }

        public void CallerMethod()
        {
            ThreadPool.QueueUserWorkItem(_ =>
            {
                {|#0:MainThreadMethod()|};
            });
        }
    }
}";

        var expected = new DiagnosticResult("MAUIMT001", DiagnosticSeverity.Warning)
            .WithLocation(0)
            .WithArguments("MainThreadMethod");

        await VerifyAnalyzerAsync(source, expected);
    }

    [Fact]
    public async Task Diagnostic_WhenMethodCalledFromTaskFactoryStartNew()
    {
        var source = @"
using System.Threading.Tasks;

namespace TestNamespace
{
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Property | System.AttributeTargets.Constructor)]
    public sealed class MainThreadOnlyAttribute : System.Attribute { }

    public class TestClass
    {
        [MainThreadOnly]
        public void MainThreadMethod() { }

        public void CallerMethod()
        {
            Task.Factory.StartNew(() =>
            {
                {|#0:MainThreadMethod()|};
            });
        }
    }
}";

        var expected = new DiagnosticResult("MAUIMT001", DiagnosticSeverity.Warning)
            .WithLocation(0)
            .WithArguments("MainThreadMethod");

        await VerifyAnalyzerAsync(source, expected);
    }

    [Fact]
    public async Task Diagnostic_WhenMethodCalledFromParallelForEach()
    {
        var source = @"
using System.Threading.Tasks;
using System.Collections.Generic;

namespace TestNamespace
{
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Property | System.AttributeTargets.Constructor)]
    public sealed class MainThreadOnlyAttribute : System.Attribute { }

    public class TestClass
    {
        [MainThreadOnly]
        public void MainThreadMethod() { }

        public void CallerMethod()
        {
            var list = new List<int> { 1, 2, 3 };
            Parallel.ForEach(list, item =>
            {
                {|#0:MainThreadMethod()|};
            });
        }
    }
}";

        var expected = new DiagnosticResult("MAUIMT001", DiagnosticSeverity.Warning)
            .WithLocation(0)
            .WithArguments("MainThreadMethod");

        await VerifyAnalyzerAsync(source, expected);
    }

    [Fact]
    public async Task NoDiagnostic_WhenMethodCalledFromMainThreadInvoke()
    {
        var source = @"
using System;
using System.Threading.Tasks;

namespace TestNamespace
{
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Property | System.AttributeTargets.Constructor)]
    public sealed class MainThreadOnlyAttribute : System.Attribute { }

    public static class MainThread
    {
        public static void BeginInvokeOnMainThread(Action action) { }
    }

    public class TestClass
    {
        [MainThreadOnly]
        public void MainThreadMethod() { }

        public void CallerMethod()
        {
            Task.Run(() =>
            {
                MainThread.BeginInvokeOnMainThread(() =>
                {
                    MainThreadMethod();
                });
            });
        }
    }
}";

        await VerifyAnalyzerAsync(source);
    }

    [Fact]
    public async Task NoDiagnostic_WhenMethodCalledFromDispatcherInvoke()
    {
        var source = @"
using System;
using System.Threading.Tasks;

namespace TestNamespace
{
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Property | System.AttributeTargets.Constructor)]
    public sealed class MainThreadOnlyAttribute : System.Attribute { }

    public class Dispatcher
    {
        public void Dispatch(Action action) { }
    }

    public class TestClass
    {
        private Dispatcher _dispatcher = new Dispatcher();

        [MainThreadOnly]
        public void MainThreadMethod() { }

        public void CallerMethod()
        {
            Task.Run(() =>
            {
                _dispatcher.Dispatch(() =>
                {
                    MainThreadMethod();
                });
            });
        }
    }
}";

        await VerifyAnalyzerAsync(source);
    }

    [Fact]
    public async Task Diagnostic_WhenMethodCalledAfterConfigureAwaitFalse()
    {
        var source = @"
using System.Threading.Tasks;

namespace TestNamespace
{
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Property | System.AttributeTargets.Constructor)]
    public sealed class MainThreadOnlyAttribute : System.Attribute { }

    public class TestClass
    {
        [MainThreadOnly]
        public void MainThreadMethod() { }

        public async Task CallerMethodAsync()
        {
            await Task.Delay(100).ConfigureAwait(false);
            {|#0:MainThreadMethod()|};
        }
    }
}";

        var expected = new DiagnosticResult("MAUIMT001", DiagnosticSeverity.Warning)
            .WithLocation(0)
            .WithArguments("MainThreadMethod");

        await VerifyAnalyzerAsync(source, expected);
    }

    [Fact]
    public async Task NoDiagnostic_WhenMethodCalledAfterConfigureAwaitTrue()
    {
        var source = @"
using System.Threading.Tasks;

namespace TestNamespace
{
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Property | System.AttributeTargets.Constructor)]
    public sealed class MainThreadOnlyAttribute : System.Attribute { }

    public class TestClass
    {
        [MainThreadOnly]
        public void MainThreadMethod() { }

        public async Task CallerMethodAsync()
        {
            await Task.Delay(100).ConfigureAwait(true);
            MainThreadMethod();
        }
    }
}";

        await VerifyAnalyzerAsync(source);
    }

    [Fact]
    public async Task Diagnostic_WhenPropertyAccessedFromTaskRun()
    {
        var source = @"
using System.Threading.Tasks;

namespace TestNamespace
{
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Property | System.AttributeTargets.Constructor)]
    public sealed class MainThreadOnlyAttribute : System.Attribute { }

    public class TestClass
    {
        [MainThreadOnly]
        public string MainThreadProperty { get; set; } = """";

        public void CallerMethod()
        {
            Task.Run(() =>
            {
                var value = {|#0:MainThreadProperty|};
            });
        }
    }
}";

        var expected = new DiagnosticResult("MAUIMT001", DiagnosticSeverity.Warning)
            .WithLocation(0)
            .WithArguments("MainThreadProperty");

        await VerifyAnalyzerAsync(source, expected);
    }

    [Fact]
    public async Task Diagnostic_WhenMethodCalledFromContinueWith()
    {
        var source = @"
using System.Threading.Tasks;

namespace TestNamespace
{
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Property | System.AttributeTargets.Constructor)]
    public sealed class MainThreadOnlyAttribute : System.Attribute { }

    public class TestClass
    {
        [MainThreadOnly]
        public void MainThreadMethod() { }

        public void CallerMethod()
        {
            Task.CompletedTask.ContinueWith(_ =>
            {
                {|#0:MainThreadMethod()|};
            });
        }
    }
}";

        var expected = new DiagnosticResult("MAUIMT001", DiagnosticSeverity.Warning)
            .WithLocation(0)
            .WithArguments("MainThreadMethod");

        await VerifyAnalyzerAsync(source, expected);
    }
}
