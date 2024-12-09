import gleam/int
import gleam/list
import gleam/string
import simplifile

pub fn parse_number(s: String) -> Int {
  let assert Ok(n) = int.base_parse(s, 10)
  n
}

pub fn read_lines(path: String) -> List(String) {
  let assert Ok(contents) = simplifile.read(path)
  let nonempty = fn(s) { s != "" }
  let lines =
    contents
    |> string.split("\n")
    |> list.filter(nonempty)
}

pub type Position {
  RowCol(i: Int, j: Int)
}

pub fn pos_add(a: Position, b: Position) -> Position {
  case a, b {
    RowCol(ai, aj), RowCol(bi, bj) -> RowCol(ai + bi, aj + bj)
  }
}

pub fn pos_sub(a: Position, b: Position) -> Position {
  case a, b {
    RowCol(ai, aj), RowCol(bi, bj) -> RowCol(ai - bi, aj - bj)
  }
}

pub fn pos_format(p: Position) -> String {
  case p {
    RowCol(i, j) -> "(" <> int.to_string(i) <> ", " <> int.to_string(j) <> ")"
  }
}
