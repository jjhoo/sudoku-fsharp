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
type Box = { Row: int; Column: int }

let newBox (row: int) (col: int) : Box =
    let r = ((row - 1) / 3) + 1
    let c = ((col - 1) / 3) + 1
    { Row=r; Column=c }

type Pos(row: int, column: int) =
    member this.Row = row
    member this.Column = column
    member this.Box = newBox row column

    override this.GetHashCode() = hash(this.Row, this.Column)
    override this.Equals(other) =
        match other with
            | :? Pos as o -> (this.Row, this.Column) = (o.Row, o.Column)
            | _ -> false

    override this.ToString() = sprintf "Pos{row: %d, column: %d}" this.Row this.Column
    interface System.IComparable with
        member this.CompareTo oobj =
            let posCompare (a: Pos) (b: Pos) : int =
                if a.Row = b.Row then compare a.Column b.Column
                else compare a.Row b.Row

            match oobj with
                | :? Pos as other -> posCompare this other
                | _ -> invalidArg "oobj" "unexpected type for oobj in CompareTo"

    member this.OnSameBox(other: Pos) = (this.Box = other.Box)
    member this.OnSameColumn(other: Pos) = (this.Column = other.Column)
    member this.OnSameRow(other: Pos) = (this.Row = other.Row)
    member this.OnSameLine(other: Pos) = this.OnSameRow(other) || this.OnSameColumn(other)
    member this.Sees(other: Pos) =
        this.OnSameRow(other) || this.OnSameColumn(other) || this.OnSameBox(other)

type CellValue = Value of int | Unsolved
type Cell(value: CellValue, pos: Pos) =
    member this.Pos = pos
    member this.Value = value

    new(value: int, row: int, col: int) =
        let num =
            match value with
                | 0 -> Unsolved
                | x -> Value x
        let pos = Pos(row, col)
        Cell(num, pos)

    new(other: Cell) =
        Cell(other.Value, other.Pos)

    override this.ToString() = sprintf "Cell{pos: %A, value: %A}" this.Pos this.Value

    member this.Conflicts(other: Cell) =
        this.Pos.Sees(other.Pos) && this.Value = other.Value

let strToGrid (grid: seq<char>) : seq<Cell> =
    let charToInt x = int x - int '0'
    // grid |> Seq.iteri (fun i x -> printfn "%d %c" i x)
    grid
    |> Seq.mapi (fun i x ->
                 let r = (i / 9) + 1
                 let c = (i % 9) + 1
                 Cell(charToInt x, r, c))

type Solver(grid: string) =
    let mutable _grid : seq<Cell> = Seq.empty
    let mutable _candidates : seq<Cell> = Seq.empty
    let mutable _solved : Map<Pos, Cell> = Map.empty

    let getCandidates (solved: Map<Pos, Cell>) : seq<Cell> =
        let gen (pos: Pos) = seq {
            for n in [1..9] do
                yield Cell(n, pos.Row, pos.Column)
        }

        let unsolved (pos: Pos) =
                if (not (solved.ContainsKey pos)) then (gen pos)
                else Seq.empty

        seq {
            for row in [1..9] do
                for col in [1..9] do
                    yield! (unsolved (Pos(row, col)))
        }

    do
        _grid <- strToGrid grid
        _solved <-
            _grid
            |> List.ofSeq
            |> List.partition (fun (cell: Cell) ->
                               match cell.Value with
                               | CellValue.Unsolved -> false
                               | CellValue.Value num -> true)
            |> fst
            |> Seq.map (fun cell -> (cell.Pos, cell))
            |> Map.ofSeq
        _candidates <- getCandidates _solved

    member this.Grid = _grid
    member this.Candidates = _candidates

    member this.RemoveConflicting(cells: seq<Cell>) =
        let filterFun (cell: Cell) : bool =
            _grid
            |> Seq.exists (fun (cell2: Cell) ->
                           (cell.Pos = cell2.Pos && cell2.Value <> CellValue.Unsolved)
                           || (cell.Pos.Sees(cell2.Pos) && cell.Value = cell2.Value))

        printfn "Old count: %d" (Seq.length _candidates)

        _candidates <-
                _candidates
                |> Seq.filter (filterFun >> not)

        printfn "Removed any? %d" (Seq.length _candidates)
        ignore

    member this.Solve() : bool =
        let (solved, unsolved) =
            _grid
            |> List.ofSeq
            |> List.partition (fun (cell: Cell) ->
                               match cell.Value with
                               | CellValue.Unsolved -> false
                               | CellValue.Value num -> true)

        let unsolvedMap =
            unsolved
            |> Seq.map (fun cell -> (cell.Pos, cell))
            |> dict

        let nsolved =
             solved
             |> Seq.ofList

        let ncandidates =
            _candidates
            |> Seq.filter (fun (cell: Cell) ->
                           unsolvedMap.ContainsKey cell.Pos)

        let removed =
            solved
            |> Seq.ofList
            |> this.RemoveConflicting
            |> ignore

        false

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

    let solver = Solver(strGrid)
    solver.Solve() |> ignore

    0
