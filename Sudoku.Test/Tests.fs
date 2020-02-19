module Tests

open System
open Xunit

open Sudoku.Solver

[<Fact>]
let ``Solver solve test`` () =
    let strGrid = "700600008800030000090000310006740005005806900400092100087000020000060009600008001"
    let solver = Solver(strGrid)
    let solved = solver.Solve()

    Assert.False(solved)
