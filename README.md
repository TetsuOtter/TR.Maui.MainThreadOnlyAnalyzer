# TR.Maui.MainThreadOnlyAnalyzer

A Roslyn analyzer for .NET MAUI that enforces main-thread-only execution by warning developers when methods decorated with `[MainThreadOnly]` are invoked from background threads, helping prevent UI-related threading issues.

[![NuGet](https://img.shields.io/nuget/v/TR.Maui.MainThreadOnlyAnalyzer.svg)](https://www.nuget.org/packages/TR.Maui.MainThreadOnlyAnalyzer/)

## Features

- **Custom Attribute Support**: Mark your methods with `[MainThreadOnly]` to indicate they should only be called from the main thread
- **Background Context Detection**: Detects calls from various background contexts:
  - `Task.Run()`
  - `Task.Factory.StartNew()`
  - `Task.ContinueWith()`
  - `ThreadPool.QueueUserWorkItem()`
  - `Parallel.ForEach()` and other `Parallel` methods
  - `ConfigureAwait(false)` continuations
- **MainThread.Invoke Detection**: Recognizes when code is wrapped in main thread dispatch methods like:
  - `MainThread.BeginInvokeOnMainThread()`
  - `Dispatcher.Dispatch()`
  - And other common dispatcher patterns
- **MAUI UI API Detection**: Automatically detects MAUI UI types and warns when they're accessed from background threads

## Installation

Install the analyzer via NuGet:

```bash
dotnet add package TR.Maui.MainThreadOnlyAnalyzer
dotnet add package TR.Maui.MainThreadOnlyAnalyzer.Attributes
```

Or via Package Manager Console:

```powershell
Install-Package TR.Maui.MainThreadOnlyAnalyzer
Install-Package TR.Maui.MainThreadOnlyAnalyzer.Attributes
```

## Usage

### Using the MainThreadOnly Attribute

Mark methods that should only be called from the main thread:

```csharp
using TR.Maui.MainThreadOnlyAnalyzer;

public class MyViewModel
{
    [MainThreadOnly]
    public void UpdateUI()
    {
        // This method should only be called from the main thread
    }
}
```

### Warnings

The analyzer will produce warning `MAUIMT001` when a `[MainThreadOnly]` method is called from a background context:

```csharp
// This will produce a warning
Task.Run(() =>
{
    UpdateUI(); // Warning MAUIMT001: Method 'UpdateUI' should only be called from the main thread
});
```

### Safe Patterns

The analyzer recognizes when you dispatch back to the main thread:

```csharp
// No warning - properly dispatched to main thread
Task.Run(() =>
{
    MainThread.BeginInvokeOnMainThread(() =>
    {
        UpdateUI(); // No warning
    });
});
```

## Diagnostic Rules

| Rule ID | Severity | Description |
|---------|----------|-------------|
| MAUIMT001 | Warning | Main thread only method called from non-main thread context |

## Requirements

- .NET 8.0 or later
- C# 10.0 or later

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
