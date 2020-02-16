namespace Sudoku.Test
open System
open NUnit.Framework

open Sudoku.Solver

[<TestFixture>]
type Test() =

    [<Test>]
    member x.TestCase() =
        let strGrid = "700600008800030000090000310006740005005806900400092100087000020000060009600008001"
        let solver = Solver(strGrid)
        let solved = solver.Solve()

        Assert.IsFalse(solved)
