import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/int
import gleam/string
import util.{type Position, RowCol}

type Shape =
  #(Int, Int)

pub type Board {
  ParsedBoard(antennae: Dict(String, List(Position)), shape: Shape)
}

pub fn parse_board(lines: List(String)) -> Board {
  let antennae =
    lines
    |> list.index_map(fn(line, i) {
      line
      |> string.to_graphemes
      |> list.index_map(fn(c, j) { #(c, RowCol(i, j)) })
      |> list.filter(fn (tup) { tup.0 != "." })
    })
    |> list.flatten
    |> list.group(fn(tup) { tup.0 })
    |> dict.map_values(fn(_, value) {
      value
      |> list.map(fn(tup) { tup.1 })
    })

  let shape = #(list.length(lines), case lines {
    [a, ..] -> string.length(a)
    _ -> 0
  })

  ParsedBoard(antennae, shape)
}

fn pair_positions_aux(
  positions: List(Position),
  acc: List(#(Position, Position)),
) -> List(#(Position, Position)) {
  case positions {
    [] -> acc
    [p1, ..rest] ->
      pair_positions_aux(
        rest,
        list.fold(positions, acc, fn(acc, p2) { [#(p1, p2), ..acc] }),
      )
  }
}

fn pair_positions(positions: List(Position)) -> List(#(Position, Position)) {
  pair_positions_aux(positions, [])
}

fn antenna_antinodes(
  p1: Position,
  p2: Position,
  board_shape: Shape,
) -> List(Position) {
  let in_bounds = fn(p: Position) -> Bool {
    let #(rows, cols) = board_shape
    let RowCol(i, j) = p
    i >= 0 && i < rows && j >= 0 && j < cols
  }

  let d = util.pos_sub(p2, p1)
  [util.pos_sub(p1, d), util.pos_add(p2, d)]
  |> list.filter(in_bounds)
}

fn format_shape(s: Shape) {
  let #(rows, cols) = s
  "(" <> int.to_string(rows) <> ", " <> int.to_string(cols) <> ")"
}

fn find_antinodes(board: Board) -> List(Position) {
  let ParsedBoard(antennae, shape) = board
  io.println("shape: " <> format_shape(shape))
  antennae
  |> dict.values
  |> list.map(fn(positions) {
    pair_positions(positions)
    |> list.fold([], fn(acc, pair) {
      case pair {
        #(p1, p2) if p1 == p2 -> acc
      #(p1, p2) -> [antenna_antinodes(p1, p2, shape), ..acc]
      }
    })
    |> list.flatten
  })
  |> list.flatten
}

pub fn count_antinodes(board: Board) {
  let antinodes =
    find_antinodes(board)
    |> list.unique

  //antinodes
  //|> list.each(fn(a) { io.println(util.pos_format(a)) })

  antinodes
  |> list.length
}

pub fn main() {
  let count = util.read_lines("inputs/day08.input")
  |> parse_board
  |> count_antinodes
  io.println("count: " <> int.to_string(count))
}
