#!/bin/bash

# Pack the analyzer project
dotnet pack src/TR.Maui.MainThreadOnlyAnalyzer/TR.Maui.MainThreadOnlyAnalyzer.csproj

# Restore packages
dotnet restore