// Copyright (c) 2018-2020 Jani J. Hakala <jjhakala@gmail.com>, Finland
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
type Box = { Row: int; Column: int }
type Pos = { Row: int; Column: int; Box: Box }
type CellValue = Value of int | Unsolved
type Cell = { Value: CellValue; Pos: Pos }

let newBox (row: int) (col: int) : Box =
    let r = ((row - 1) / 3) + 1
    let c = ((col - 1) / 3) + 1
    { Row=r; Column=c }

let newPos (row: int) (col: int) : Pos =
    let box = newBox row col
    { Row=row; Column=col; Box=box }

let newCell (value: int) (row: int) (col: int) : Cell =
    let pos = newPos row col
    match value with
        | 0 -> { Value=Unsolved; Pos=pos }
        | _ -> { Value=Value value; Pos=pos }

let initCandidates : seq<Cell> =
    let cs = seq { for row in [1..9] do
                   for col in [1..9] do
                   for n in [1..9] do
                   yield newCell n row col }
    cs

let strToGrid (grid: seq<char>) : seq<Cell> =
    let charToInt x = int x - int '0'
    // grid |> Seq.iteri (fun i x -> printfn "%d %c" i x)
    grid
    |> Seq.mapi (fun i x ->
                 let r = (i / 9) + 1
                 let c = (i % 9) + 1
                 newCell (charToInt x) r c)

[<EntryPoint>]
let main argv =
    printfn "Hello world %A" argv

    let strGrid = "700600008800030000090000310006740005005806900400092100087000020000060009600008001"
    let grid = strToGrid strGrid

    let (solved, unsolved) =
        grid
        |> List.ofSeq
        |> List.partition (fun cell ->
                           match cell.Value with
                           | Unsolved -> false
                           | other -> true)

    let solvedMap =
        solved
        |> Seq.map (fun cell -> (cell.Pos, cell))
        |> dict

    let cands = initCandidates

    let candMap =
        cands
        |> Seq.groupBy (fun (cell: Cell) -> cell.Pos)
        |> dict

    printfn "    cell %A" (candMap.Item((newPos 1 1)))

    0
