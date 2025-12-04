using System.Threading.Tasks;
using TR.Maui.MainThreadOnlyAnalyzer;

namespace TR.Maui.MainThreadOnlyAnalyzer.Sample;

/// <summary>
/// Sample service demonstrating the MainThreadOnlyAnalyzer functionality.
/// Open this file in Visual Studio or VS Code with C# extension to see analyzer warnings.
/// </summary>
public class SampleService
{
    /// <summary>
    /// A method marked with [MainThreadOnly] that should only be called from the main thread.
    /// </summary>
    [MainThreadOnly]
    public void UpdateUI()
    {
        // This method simulates UI updates that must happen on the main thread
        Console.WriteLine("Updating UI on main thread");
    }

    /// <summary>
    /// A property marked with [MainThreadOnly] that should only be accessed from the main thread.
    /// </summary>
    [MainThreadOnly]
    public string UIState { get; set; } = "Initial";

    /// <summary>
    /// This method demonstrates CORRECT usage - calling from main thread context.
    /// No analyzer warning should appear here.
    /// </summary>
    public void CorrectUsage_DirectCall()
    {
        // Direct call - no warning (we're presumably on the main thread)
        UpdateUI();
    }

    /// <summary>
    /// This method demonstrates INCORRECT usage - calling from Task.Run.
    /// The analyzer should produce warning MAUIMT001.
    /// </summary>
    public void IncorrectUsage_TaskRun()
    {
        // WARNING: MAUIMT001 - Method 'UpdateUI' should only be called from the main thread
        Task.Run(() =>
        {
            UpdateUI(); // This should trigger a warning!
        });
    }

    /// <summary>
    /// This method demonstrates INCORRECT usage - calling from ThreadPool.
    /// The analyzer should produce warning MAUIMT001.
    /// </summary>
    public void IncorrectUsage_ThreadPool()
    {
        // WARNING: MAUIMT001 - Method 'UpdateUI' should only be called from the main thread
        ThreadPool.QueueUserWorkItem(_ =>
        {
            UpdateUI(); // This should trigger a warning!
        });
    }

    /// <summary>
    /// This method demonstrates INCORRECT usage - accessing property from background thread.
    /// The analyzer should produce warning MAUIMT001.
    /// </summary>
    public void IncorrectUsage_PropertyAccess()
    {
        // WARNING: MAUIMT001 - Property 'UIState' should only be accessed from the main thread
        Task.Run(() =>
        {
            var state = UIState; // This should trigger a warning!
            Console.WriteLine(state);
        });
    }

    /// <summary>
    /// This method demonstrates INCORRECT usage - calling after ConfigureAwait(false).
    /// The analyzer should produce warning MAUIMT001.
    /// </summary>
    public async Task IncorrectUsage_ConfigureAwaitFalse()
    {
        await Task.Delay(100).ConfigureAwait(false);
        // WARNING: MAUIMT001 - After ConfigureAwait(false), we may be on a background thread
        UpdateUI(); // This should trigger a warning!
    }

    /// <summary>
    /// This method demonstrates CORRECT usage - properly dispatching to main thread.
    /// The analyzer should NOT produce a warning here because we're using MainThread dispatch.
    /// </summary>
    public void CorrectUsage_WithMainThreadDispatch()
    {
        Task.Run(() =>
        {
            // Using MainThread.BeginInvokeOnMainThread to properly dispatch to main thread
            // Note: In a real MAUI app, this would use the actual MainThread class
            SampleMainThread.BeginInvokeOnMainThread(() =>
            {
                UpdateUI(); // No warning - properly dispatched to main thread
            });
        });
    }

    /// <summary>
    /// This method demonstrates CORRECT usage - using ConfigureAwait(true).
    /// No analyzer warning should appear here because we stay on the original context.
    /// </summary>
    public async Task CorrectUsage_ConfigureAwaitTrue()
    {
        await Task.Delay(100).ConfigureAwait(true);
        UpdateUI(); // No warning - we're back on the original (main) thread
    }
}

/// <summary>
/// Mock MainThread class to simulate MAUI's MainThread for testing purposes.
/// In a real MAUI application, you would use Microsoft.Maui.ApplicationModel.MainThread.
/// </summary>
public static class SampleMainThread
{
    public static void BeginInvokeOnMainThread(Action action)
    {
        // In a real MAUI app, this would dispatch to the main thread
        action();
    }
}
