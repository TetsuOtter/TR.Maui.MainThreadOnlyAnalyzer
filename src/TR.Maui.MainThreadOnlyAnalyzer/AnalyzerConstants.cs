using System;
using System.Collections.Generic;

namespace TR.Maui.MainThreadOnlyAnalyzer;

/// <summary>
/// Contains constant values used by the MainThreadOnlyAnalyzer.
/// </summary>
internal static class AnalyzerConstants
{
    /// <summary>
    /// The diagnostic ID for main thread only violations.
    /// </summary>
    public const string DiagnosticId = "MAUIMT001";

    /// <summary>
    /// The category for threading-related diagnostics.
    /// </summary>
    public const string Category = "Threading";

    /// <summary>
    /// Known MAUI UI types that should only be accessed from the main thread.
    /// </summary>
    public static readonly HashSet<string> MauiUiTypeNames = new(StringComparer.Ordinal)
    {
        // Core MAUI UI elements
        "Microsoft.Maui.Controls.View",
        "Microsoft.Maui.Controls.Layout",
        "Microsoft.Maui.Controls.Page",
        "Microsoft.Maui.Controls.VisualElement",
        "Microsoft.Maui.Controls.Element",
        "Microsoft.Maui.Controls.NavigableElement",
        "Microsoft.Maui.Controls.Application",
        "Microsoft.Maui.Controls.Shell",
        "Microsoft.Maui.Controls.Window",
        "Microsoft.Maui.Controls.ContentView",
        "Microsoft.Maui.Controls.ContentPage",
        "Microsoft.Maui.Controls.NavigationPage",
        "Microsoft.Maui.Controls.TabbedPage",
        "Microsoft.Maui.Controls.FlyoutPage",
        "Microsoft.Maui.Controls.TemplatedPage",
        "Microsoft.Maui.Controls.TemplatedView",
        
        // Common controls
        "Microsoft.Maui.Controls.Label",
        "Microsoft.Maui.Controls.Button",
        "Microsoft.Maui.Controls.Entry",
        "Microsoft.Maui.Controls.Editor",
        "Microsoft.Maui.Controls.Image",
        "Microsoft.Maui.Controls.ImageButton",
        "Microsoft.Maui.Controls.Picker",
        "Microsoft.Maui.Controls.DatePicker",
        "Microsoft.Maui.Controls.TimePicker",
        "Microsoft.Maui.Controls.Slider",
        "Microsoft.Maui.Controls.Stepper",
        "Microsoft.Maui.Controls.Switch",
        "Microsoft.Maui.Controls.CheckBox",
        "Microsoft.Maui.Controls.ProgressBar",
        "Microsoft.Maui.Controls.ActivityIndicator",
        "Microsoft.Maui.Controls.SearchBar",
        "Microsoft.Maui.Controls.WebView",
        "Microsoft.Maui.Controls.RefreshView",
        "Microsoft.Maui.Controls.ScrollView",
        "Microsoft.Maui.Controls.CarouselView",
        "Microsoft.Maui.Controls.CollectionView",
        "Microsoft.Maui.Controls.ListView",
        "Microsoft.Maui.Controls.TableView",
        "Microsoft.Maui.Controls.StackLayout",
        "Microsoft.Maui.Controls.Grid",
        "Microsoft.Maui.Controls.AbsoluteLayout",
        "Microsoft.Maui.Controls.FlexLayout",
        "Microsoft.Maui.Controls.Frame",
        "Microsoft.Maui.Controls.Border",
        "Microsoft.Maui.Controls.BoxView",
        "Microsoft.Maui.Controls.RadioButton",
        "Microsoft.Maui.Controls.Span",
        "Microsoft.Maui.Controls.FormattedString",
        
        // Graphics
        "Microsoft.Maui.Controls.Shapes.Shape",
        "Microsoft.Maui.Graphics.ICanvas",
    };

    /// <summary>
    /// Method names on MAUI types that require main thread access.
    /// </summary>
    public static readonly HashSet<string> MauiMainThreadMethods = new(StringComparer.Ordinal)
    {
        // Navigation methods
        "PushAsync",
        "PopAsync",
        "PushModalAsync",
        "PopModalAsync",
        "PopToRootAsync",
        "GoToAsync",
        
        // UI update methods
        "InvalidateMeasure",
        "InvalidateArrange",
        "InvalidateLayout",
        "ForceLayout",
        "BatchBegin",
        "BatchCommit",
        "Focus",
        "Unfocus",
        
        // Animation methods
        "Animate",
        "AbortAnimation",
        "FadeTo",
        "RotateTo",
        "RelRotateTo",
        "RotateXTo",
        "RotateYTo",
        "ScaleTo",
        "RelScaleTo",
        "ScaleXTo",
        "ScaleYTo",
        "TranslateTo",
        "ColorTo",
        "LayoutTo",
    };

    /// <summary>
    /// Types that indicate non-main thread context.
    /// </summary>
    public static readonly HashSet<string> BackgroundContextTypes = new(StringComparer.Ordinal)
    {
        "System.Threading.Tasks.Task",
        "System.Threading.Thread",
        "System.Threading.ThreadPool",
        "System.Threading.Timer",
        "System.ComponentModel.BackgroundWorker",
    };

    /// <summary>
    /// Methods that start background execution.
    /// </summary>
    public static readonly HashSet<string> BackgroundStartMethods = new(StringComparer.Ordinal)
    {
        // Task methods
        "Run",
        "Factory.StartNew",
        "StartNew",
        "ContinueWith",
        
        // Thread methods  
        "Start",
        
        // ThreadPool methods
        "QueueUserWorkItem",
        "UnsafeQueueUserWorkItem",
        "RegisterWaitForSingleObject",
        
        // Timer constructor handled separately
    };

    /// <summary>
    /// Methods that switch back to main thread.
    /// </summary>
    public static readonly HashSet<string> MainThreadInvokeMethods = new(StringComparer.Ordinal)
    {
        // MAUI MainThread
        "BeginInvokeOnMainThread",
        "InvokeOnMainThreadAsync",
        
        // Xamarin/Legacy
        "InvokeOnMainThread",
        
        // Dispatcher
        "Dispatch",
        "DispatchAsync",
        "TryEnqueue",
        "Invoke",
        "InvokeAsync",
        "BeginInvoke",
    };
}
