module Tests

open System
open Xunit

open Sudoku.Solver

[<Fact>]
let ``Solver solve test (naked pairs)`` () =
    let strGrid = "400000938032094100095300240370609004529001673604703090957008300003900400240030709"
    let solver = Solver(strGrid)
    let solved = solver.Solve()

    Assert.False(solved)

[<Fact>]
let ``Solver solve test (naked triples)`` () =
    let strGrid = "070008029002000004854020000008374200000000000003261700000090612200000400130600070"
    let solver = Solver(strGrid)
    let solved = solver.Solve()

    Assert.False(solved)

[<Fact>]
let ``Solver solve test (hidden pairs)`` () =
    let strGrid = "000000000904607000076804100309701080008000300050308702007502610000403208000000000"
    let solver = Solver(strGrid)
    let solved = solver.Solve()

    Assert.False(solved)

[<Fact>]
let ``Solver solve test (hidden triples)`` () =
    let strGrid = "000000000231090000065003100008924000100050006000136700009300570000010843000000000"
    let solver = Solver(strGrid)
    let solved = solver.Solve()

    Assert.False(solved)
