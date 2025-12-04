using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace TR.Maui.MainThreadOnlyAnalyzer;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public class MainThreadOnlyAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = AnalyzerConstants.DiagnosticId;

    private static readonly LocalizableString Title = "Main thread only method called from non-main thread context";
    private static readonly LocalizableString MessageFormat = "Method '{0}' should only be called from the main thread";
    private static readonly LocalizableString Description = "This method is marked as main-thread-only or is a MAUI UI API and should not be called from background threads.";

    private static readonly DiagnosticDescriptor Rule = new(
        DiagnosticId,
        Title,
        MessageFormat,
        AnalyzerConstants.Category,
        DiagnosticSeverity.Warning,
        isEnabledByDefault: true,
        description: Description);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

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

        // Check if the method is a known main-thread method on a MAUI type
        if (AnalyzerConstants.MauiMainThreadMethods.Contains(method.Name) && IsMauiUiType(containingType))
            return true;

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
            if (AnalyzerConstants.MauiUiTypeNames.Contains(fullName))
                return true;

            // Check interfaces using explicit filter
            if (type.AllInterfaces.Any(iface => AnalyzerConstants.MauiUiTypeNames.Contains(iface.ToDisplayString())))
                return true;

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

        // First, check if we're in a statement after ConfigureAwait(false)
        if (IsAfterConfigureAwaitFalse(node, semanticModel))
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
        
        // If statement not found or is the first statement, there's nothing before it
        if (nodeIndex < 0)
            return false;
        
        // If this is the first statement (index 0), there are no prior statements to check
        if (nodeIndex == 0)
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
        if (AnalyzerConstants.MainThreadInvokeMethods.Contains(method.Name))
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

                // Note: ConfigureAwait(false) is handled by IsAfterConfigureAwaitFalse 
                // which performs sequential flow analysis rather than ancestry check
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

        // Note: We don't check lambdas here anymore - they're implicitly covered
        // when we detect the parent invocation (Task.Run, etc.)
        
        return false;
    }
}
