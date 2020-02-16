# A Sudoku puzzle solver

An implementation based on logic.

Work in progress.

## Test run

Assuming debian/buster as the programming environment with mono repository as

    deb https://download.mono-project.com/repo/debian buster main

and packages *mono-complete*, *fsharp*, and *nuget* having been installed,

    nuget restore
    msbuild
    mono Sudoku.App/bin/Debug/Sudoku.App.exe test

    nuget install NUnit.Console -Version 3.9.0 -OutputDirectory testrunner
    mono testrunner/NUnit.ConsoleRunner.3.9.0/tools/nunit3-console.exe Sudoku.Test/bin/Debug/Sudoku.Test.dll

# Testing

Travis-ci: [![Build status](https://travis-ci.org/jjhoo/sudoku-fsharp.svg?branch=master)](https://travis-ci.org/jjhoo/sudoku-fsharp)
