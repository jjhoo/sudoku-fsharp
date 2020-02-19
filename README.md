# A Sudoku puzzle solver

An implementation based on logic.

Work in progress.

# Test run with dotnet

Assuming debian/buster as the programming environment with dotnet repository as

    deb [arch=amd64,arm64,armhf] https://packages.microsoft.com/debian/10/prod buster main

and package *dotnet-sdk-3.1* having been install,

    dotnet restore
    dotnet build
    dotnet test Sudoku.Test/Sudoku.Test.fsproj


# Testing

Travis-ci: [![Build status](https://travis-ci.org/jjhoo/sudoku-fsharp.svg?branch=master)](https://travis-ci.org/jjhoo/sudoku-fsharp)
