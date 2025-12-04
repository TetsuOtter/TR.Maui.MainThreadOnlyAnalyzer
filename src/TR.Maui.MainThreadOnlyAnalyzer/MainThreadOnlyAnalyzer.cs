using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace TR.Maui.MainThreadOnlyAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MainThreadOnlyAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "MAUIMT001";
        private const string Category = "Threading";

        private static readonly LocalizableString Title = "Main thread only method called from non-main thread context";
        private static readonly LocalizableString MessageFormat = "Method '{0}' should only be called from the main thread";
        private static readonly LocalizableString Description = "This method is marked as main-thread-only or is a MAUI UI API and should not be called from background threads.";

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(
            DiagnosticId,
            Title,
            MessageFormat,
            Category,
            DiagnosticSeverity.Warning,
            isEnabledByDefault: true,
            description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        /// <summary>
        /// Known MAUI UI types that should only be accessed from the main thread.
        /// </summary>
        private static readonly HashSet<string> MauiUiTypeNames = new HashSet<string>(StringComparer.Ordinal)
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
        private static readonly HashSet<string> MauiMainThreadMethods = new HashSet<string>(StringComparer.Ordinal)
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
        private static readonly HashSet<string> BackgroundContextTypes = new HashSet<string>(StringComparer.Ordinal)
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
        private static readonly HashSet<string> BackgroundStartMethods = new HashSet<string>(StringComparer.Ordinal)
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
        private static readonly HashSet<string> MainThreadInvokeMethods = new HashSet<string>(StringComparer.Ordinal)
        {
            // MAUI MainThread
            "BeginInvokeOnMainThread",
            "InvokeOnMainThreadAsync",
            
            // Xamarin/Legacy
            "BeginInvokeOnMainThread",
            "InvokeOnMainThread",
            
            // Dispatcher
            "Dispatch",
            "DispatchAsync",
            "TryEnqueue",
            "Invoke",
            "InvokeAsync",
            "BeginInvoke",
        };

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();

            context.RegisterSyntaxNodeAction(AnalyzeInvocation, SyntaxKind.InvocationExpression);
            context.RegisterSyntaxNodeAction(AnalyzeMemberAccess, SyntaxKind.SimpleMemberAccessExpression);
            context.RegisterSyntaxNodeAction(AnalyzeIdentifier, SyntaxKind.IdentifierName);
        }

        private void AnalyzeInvocation(SyntaxNodeAnalysisContext context)
        {
            var invocation = (InvocationExpressionSyntax)context.Node;
            
            var symbolInfo = context.SemanticModel.GetSymbolInfo(invocation, context.CancellationToken);
            if (symbolInfo.Symbol is not IMethodSymbol methodSymbol)
                return;

            if (!IsMainThreadOnlyMethod(methodSymbol, context.SemanticModel.Compilation))
                return;

            if (IsInNonMainThreadContext(invocation, context.SemanticModel))
            {
                var diagnostic = Diagnostic.Create(Rule, invocation.GetLocation(), methodSymbol.Name);
                context.ReportDiagnostic(diagnostic);
            }
        }

        private void AnalyzeMemberAccess(SyntaxNodeAnalysisContext context)
        {
            var memberAccess = (MemberAccessExpressionSyntax)context.Node;
            
            // Skip if this is part of an invocation (handled by AnalyzeInvocation)
            if (memberAccess.Parent is InvocationExpressionSyntax)
                return;

            var symbolInfo = context.SemanticModel.GetSymbolInfo(memberAccess, context.CancellationToken);
            if (symbolInfo.Symbol is not IPropertySymbol propertySymbol)
                return;

            if (!IsMainThreadOnlyProperty(propertySymbol, context.SemanticModel.Compilation))
                return;

            if (IsInNonMainThreadContext(memberAccess, context.SemanticModel))
            {
                var diagnostic = Diagnostic.Create(Rule, memberAccess.GetLocation(), propertySymbol.Name);
                context.ReportDiagnostic(diagnostic);
            }
        }

        private void AnalyzeIdentifier(SyntaxNodeAnalysisContext context)
        {
            var identifier = (IdentifierNameSyntax)context.Node;
            
            // Skip if this is part of a member access expression (handled by AnalyzeMemberAccess)
            if (identifier.Parent is MemberAccessExpressionSyntax memberAccess && memberAccess.Name == identifier)
                return;
            
            // Skip if this is part of an invocation expression (handled by AnalyzeInvocation)
            if (identifier.Parent is InvocationExpressionSyntax)
                return;

            var symbolInfo = context.SemanticModel.GetSymbolInfo(identifier, context.CancellationToken);
            if (symbolInfo.Symbol is not IPropertySymbol propertySymbol)
                return;

            if (!IsMainThreadOnlyProperty(propertySymbol, context.SemanticModel.Compilation))
                return;

            if (IsInNonMainThreadContext(identifier, context.SemanticModel))
            {
                var diagnostic = Diagnostic.Create(Rule, identifier.GetLocation(), propertySymbol.Name);
                context.ReportDiagnostic(diagnostic);
            }
        }

        private static bool IsMainThreadOnlyMethod(IMethodSymbol method, Compilation compilation)
        {
            // Check for MainThreadOnlyAttribute
            if (HasMainThreadOnlyAttribute(method))
                return true;

            // Check if it's a MAUI UI method
            if (IsMauiUiMethod(method))
                return true;

            return false;
        }

        private static bool IsMainThreadOnlyProperty(IPropertySymbol property, Compilation compilation)
        {
            // Check for MainThreadOnlyAttribute on the property
            if (HasMainThreadOnlyAttribute(property))
                return true;

            // Check if it's a property on a MAUI UI type
            if (IsMauiUiProperty(property))
                return true;

            return false;
        }

        private static bool HasMainThreadOnlyAttribute(ISymbol symbol)
        {
            return symbol.GetAttributes().Any(attr =>
                attr.AttributeClass?.Name == "MainThreadOnlyAttribute" ||
                attr.AttributeClass?.ToDisplayString() == "TR.Maui.MainThreadOnlyAnalyzer.MainThreadOnlyAttribute");
        }

        private static bool IsMauiUiMethod(IMethodSymbol method)
        {
            var containingType = method.ContainingType;
            if (containingType == null)
                return false;

            // Check if the method is a known main-thread method
            if (MauiMainThreadMethods.Contains(method.Name))
            {
                // Verify it's on a MAUI type
                if (IsMauiUiType(containingType))
                    return true;
            }

            // Check if it's a member of a MAUI UI type (all members should be main thread)
            return IsMauiUiType(containingType);
        }

        private static bool IsMauiUiProperty(IPropertySymbol property)
        {
            var containingType = property.ContainingType;
            return containingType != null && IsMauiUiType(containingType);
        }

        private static bool IsMauiUiType(ITypeSymbol? type)
        {
            while (type != null)
            {
                var fullName = type.ToDisplayString();
                if (MauiUiTypeNames.Contains(fullName))
                    return true;

                // Check interfaces
                foreach (var iface in type.AllInterfaces)
                {
                    if (MauiUiTypeNames.Contains(iface.ToDisplayString()))
                        return true;
                }

                type = type.BaseType;
            }

            return false;
        }

        private static bool IsInNonMainThreadContext(SyntaxNode node, SemanticModel semanticModel)
        {
            // Walk up the syntax tree to find context indicators
            var current = node.Parent;
            int mainThreadInvokeDepth = 0;
            int backgroundContextDepth = 0;
            bool foundConfigureAwaitFalse = false;

            // First, check if we're in a statement after ConfigureAwait(false)
            foundConfigureAwaitFalse = IsAfterConfigureAwaitFalse(node, semanticModel);
            if (foundConfigureAwaitFalse)
            {
                backgroundContextDepth++;
            }

            while (current != null)
            {
                // Check for MainThread.Invoke and similar patterns
                if (IsMainThreadInvokeContext(current, semanticModel))
                {
                    mainThreadInvokeDepth++;
                }

                // Check for background thread contexts
                if (IsBackgroundThreadContext(current, semanticModel))
                {
                    backgroundContextDepth++;
                }

                current = current.Parent;
            }

            // If we're in a background context but not wrapped in a main thread invoke, warn
            return backgroundContextDepth > mainThreadInvokeDepth;
        }

        private static bool IsAfterConfigureAwaitFalse(SyntaxNode node, SemanticModel semanticModel)
        {
            // Find the containing method or lambda
            var containingMethod = node.FirstAncestorOrSelf<MethodDeclarationSyntax>();
            var containingLambda = node.FirstAncestorOrSelf<LambdaExpressionSyntax>();
            
            SyntaxNode? methodBody = containingMethod?.Body as SyntaxNode ?? containingMethod?.ExpressionBody;
            if (containingLambda != null)
            {
                methodBody = containingLambda.Body;
            }
            
            if (methodBody == null)
                return false;

            // Get the containing statement
            var containingStatement = node.FirstAncestorOrSelf<StatementSyntax>();
            if (containingStatement == null)
                return false;

            // Look for await expressions with ConfigureAwait(false) that come before this statement
            var statements = methodBody.DescendantNodes().OfType<StatementSyntax>().ToList();
            var nodeIndex = statements.IndexOf(containingStatement);
            
            if (nodeIndex <= 0)
                return false;

            // Check statements before the current one
            for (int i = 0; i < nodeIndex; i++)
            {
                var stmt = statements[i];
                var awaits = stmt.DescendantNodes().OfType<AwaitExpressionSyntax>();
                foreach (var awaitExpr in awaits)
                {
                    if (HasConfigureAwaitFalse(awaitExpr, semanticModel))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        private static bool HasConfigureAwaitFalse(AwaitExpressionSyntax awaitExpr, SemanticModel semanticModel)
        {
            if (awaitExpr.Expression is InvocationExpressionSyntax configureAwaitCall)
            {
                var symbolInfo = semanticModel.GetSymbolInfo(configureAwaitCall);
                if (symbolInfo.Symbol is IMethodSymbol method && method.Name == "ConfigureAwait")
                {
                    var args = configureAwaitCall.ArgumentList.Arguments;
                    if (args.Count > 0 && args[0].Expression is LiteralExpressionSyntax literal &&
                        literal.Kind() == SyntaxKind.FalseLiteralExpression)
                    {
                        return true;
                    }
                }
            }
            return false;
        }

        private static bool IsMainThreadInvokeContext(SyntaxNode node, SemanticModel semanticModel)
        {
            if (node is not InvocationExpressionSyntax invocation)
                return false;

            var symbolInfo = semanticModel.GetSymbolInfo(invocation);
            if (symbolInfo.Symbol is not IMethodSymbol method)
                return false;

            // Check for MainThread.BeginInvokeOnMainThread, Dispatcher.Dispatch, etc.
            if (MainThreadInvokeMethods.Contains(method.Name))
            {
                // Accept any class named MainThread or Dispatcher or containing those
                var containingTypeName = method.ContainingType?.Name ?? "";
                var fullTypeName = method.ContainingType?.ToDisplayString() ?? "";
                
                if (containingTypeName == "MainThread" ||
                    containingTypeName == "Dispatcher" ||
                    containingTypeName == "Device" ||
                    fullTypeName.Contains("MainThread") ||
                    fullTypeName.Contains("Dispatcher") ||
                    fullTypeName.Contains("SynchronizationContext"))
                {
                    return true;
                }
            }

            return false;
        }

        private static bool IsBackgroundThreadContext(SyntaxNode node, SemanticModel semanticModel)
        {
            // Check for Task.Run, Thread.Start, ThreadPool.QueueUserWorkItem, etc.
            if (node is InvocationExpressionSyntax invocation)
            {
                var symbolInfo = semanticModel.GetSymbolInfo(invocation);
                if (symbolInfo.Symbol is IMethodSymbol method)
                {
                    var containingType = method.ContainingType?.ToDisplayString() ?? "";
                    
                    // Task.Run
                    if (containingType == "System.Threading.Tasks.Task" && method.Name == "Run")
                        return true;

                    // Task.Factory.StartNew
                    if (containingType == "System.Threading.Tasks.TaskFactory" && method.Name == "StartNew")
                        return true;

                    // Task.ContinueWith (without proper scheduler)
                    if (method.Name == "ContinueWith" && containingType.StartsWith("System.Threading.Tasks.Task"))
                        return true;

                    // ThreadPool.QueueUserWorkItem
                    if (containingType == "System.Threading.ThreadPool" && 
                        (method.Name == "QueueUserWorkItem" || method.Name == "UnsafeQueueUserWorkItem"))
                        return true;

                    // Parallel methods
                    if (containingType == "System.Threading.Tasks.Parallel")
                        return true;

                    // ConfigureAwait(false)
                    if (method.Name == "ConfigureAwait")
                    {
                        var args = invocation.ArgumentList.Arguments;
                        if (args.Count > 0)
                        {
                            var firstArg = args[0].Expression;
                            if (firstArg is LiteralExpressionSyntax literal && 
                                literal.Kind() == SyntaxKind.FalseLiteralExpression)
                            {
                                return true;
                            }
                        }
                    }
                }
            }

            // Check for new Thread()
            if (node is ObjectCreationExpressionSyntax creation)
            {
                var symbolInfo = semanticModel.GetSymbolInfo(creation);
                if (symbolInfo.Symbol is IMethodSymbol ctor)
                {
                    var typeName = ctor.ContainingType?.ToDisplayString() ?? "";
                    if (typeName == "System.Threading.Thread" || 
                        typeName == "System.Threading.Timer" ||
                        typeName == "System.ComponentModel.BackgroundWorker")
                    {
                        return true;
                    }
                }
            }

            // Check for lambda/delegate passed to background methods
            if (node is LambdaExpressionSyntax || node is AnonymousMethodExpressionSyntax)
            {
                var parent = node.Parent;
                while (parent != null && parent is not ArgumentSyntax)
                {
                    parent = parent.Parent;
                }

                if (parent is ArgumentSyntax arg && arg.Parent is ArgumentListSyntax argList && 
                    argList.Parent is InvocationExpressionSyntax parentInvocation)
                {
                    return IsBackgroundThreadContext(parentInvocation, semanticModel);
                }
            }

            return false;
        }
    }
}
