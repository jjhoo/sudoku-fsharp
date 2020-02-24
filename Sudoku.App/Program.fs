// Copyright (c) 2020 Jani J. Hakala <jjhakala@gmail.com>, Finland
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Affero General Public License as
//  published by the Free Software Foundation, version 3 of the
//  License.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Affero General Public License for more details.
//
//  You should have received a copy of the GNU Affero General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

namespace Sudoku

module Program =
    open Combinations
    open Permutations
    open Types
    open Solver

    [<EntryPoint>]
    let main argv =
        printfn "Hello world %A" argv

        let strGrid = "000000000231090000065003100008924000100050006000136700009300570000010843000000000"
        let solver = Solver(strGrid)
        solver.Solve() |> ignore

        0
