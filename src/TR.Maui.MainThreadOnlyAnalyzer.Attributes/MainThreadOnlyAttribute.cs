using System;

namespace TR.Maui.MainThreadOnlyAnalyzer
{
    /// <summary>
    /// Indicates that the method should only be called from the main (UI) thread.
    /// The analyzer will warn when this method is called from a potentially non-main thread context.
    /// </summary>
    [AttributeUsage(AttributeTargets.Method | AttributeTargets.Property | AttributeTargets.Constructor, Inherited = true, AllowMultiple = false)]
    public sealed class MainThreadOnlyAttribute : Attribute
    {
        /// <summary>
        /// Creates a new instance of <see cref="MainThreadOnlyAttribute"/>.
        /// </summary>
        public MainThreadOnlyAttribute() { }
    }
}
