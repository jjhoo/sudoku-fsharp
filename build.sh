#!/bin/sh

nuget restore
msbuild
mono ./Sudoku/obj/x86/Debug/Sudoku.exe test