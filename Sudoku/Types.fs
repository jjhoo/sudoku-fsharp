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

module Types =
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

    type FindResult = { Solved: seq<Cell>; Eliminated: seq<Cell> }
