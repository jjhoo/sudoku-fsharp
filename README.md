# A Sudoku puzzle solver

An implementation based on logic.

Work in progress.

## Test run

Assuming debian/buster as the programming environment with mono repository as

    deb https://download.mono-project.com/repo/debian buster main

and packages *mono-complete*, *fsharp*, and *nuget* having been installed,

    nuget restore
    msbuild
    mono ./Sudoku/obj/x86/Debug/Sudoku.exe test
