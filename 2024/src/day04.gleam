import gleam/bool
import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/option.{Some}
import gleam/regexp.{Match}
import gleam/result
import gleam/string
import gleam/yielder
import glearray.{type Array}
import simplifile

fn parse_number(s: String) -> Int {
  let assert Ok(n) = int.base_parse(s, 10)
  n
}

fn parse_line(line: String) -> List(Int) {
  line
  |> string.to_graphemes()
  |> list.map(fn(c) {
    case c {
      "X" -> 1
      "M" -> 2
      "A" -> 3
      "S" -> 4
      _ -> 0
    }
  })
}

type Matrix(a) =
  Array(Array(a))

pub fn to_matrix(line_strings: List(String)) -> Matrix(Int) {
  line_strings
  |> list.map(parse_line)
  |> list.map(glearray.from_list)
  |> glearray.from_list
}

fn read_input(input: String) -> Matrix(Int) {
  let assert Ok(contents) = simplifile.read(input)
  contents
  |> string.split("\n")
  |> list.filter(fn(line) { line != "" })
  |> to_matrix
}

pub fn mirror_vertical(m: Matrix(Int)) -> Matrix(Int) {
  m
  |> glearray.to_list()
  |> list.map(fn(row) {
    row
    |> glearray.to_list()
    |> list.reverse()
    |> glearray.from_list()
  })
  |> glearray.from_list()
}

pub fn mirror_horizontal(m: Matrix(Int)) -> Matrix(Int) {
  m
  |> glearray.to_list()
  |> list.reverse()
  |> glearray.from_list()
}

pub fn matrix_to_list(m: Matrix(Int)) -> List(List(Int)) {
  m
  |> glearray.to_list()
  |> list.map(glearray.to_list)
}

pub fn matrix_shape(m: Matrix(Int)) -> #(Int, Int) {
  case glearray.length(m) {
    0 -> #(0, 0)
    rows -> {
      let assert Ok(first) =
        m
        |> glearray.get(0)
      let cols = glearray.length(first)
      #(rows, cols)
    }
  }
}

pub fn matrix_get(m: Matrix(Int), i: Int, j: Int) -> Result(Int, Nil) {
  use row <- result.try(
    m
    |> glearray.get(i),
  )
  row
  |> glearray.get(j)
}

pub fn matrix_transpose(m: Matrix(Int)) -> Matrix(Int) {
  let #(rows, cols) = matrix_shape(m)
  yielder.range(0, cols - 1)
  |> yielder.map(fn(col) {
    yielder.range(0, rows - 1)
    |> yielder.map(fn(row) {
      let assert Ok(x) = matrix_get(m, row, col)
      x
    })
    |> yielder.to_list()
    |> glearray.from_list()
  })
  |> yielder.to_list()
  |> glearray.from_list()
}

fn diagonals_down_right(m: Matrix(Int)) -> List(List(Int)) {
  let #(rows, cols) = matrix_shape(m)

  let diagonal = fn(m: Matrix(Int), start_row: Int, start_col: Int) -> List(Int) {
    yielder.range(0, int.min(rows, cols) - 1)
    |> yielder.fold([], fn(acc, i) {
      case matrix_get(m, start_row + i, start_col + i) {
        Ok(x) -> [x, ..acc]
        _ -> acc
      }
    })
    |> list.reverse()
  }

  yielder.range(0, rows - 1)
  |> yielder.map(fn(row) { diagonal(m, row, 0) })
  |> yielder.append(
    // skip col=0 because it was covered above
    yielder.range(1, cols - 1)
    |> yielder.map(fn(col) { diagonal(m, 0, col) }),
  )
  |> yielder.to_list()
}

fn diagonals_down_left(m: Matrix(Int)) -> List(List(Int)) {
  let m_vertical = mirror_vertical(m)
  diagonals_down_right(m_vertical)
}

fn diagonals_up_right(m: Matrix(Int)) -> List(List(Int)) {
  diagonals_down_left(m)
  |> list.map(list.reverse)
}

fn diagonals_up_left(m: Matrix(Int)) -> List(List(Int)) {
  diagonals_down_right(m)
  |> list.map(list.reverse)
}

pub fn diagonals(m: Matrix(Int)) -> List(Array(Int)) {
  let down_right = diagonals_down_right(m)
  let down_left = diagonals_down_left(m)
  list.flatten([
    // top left -> bottom right
    down_right,
    // top right -> bottom left
    down_left,
    // bottom left -> top right
    down_left
      |> list.map(list.reverse),
    // bottom right -> top left
    down_right
      |> list.map(list.reverse),
  ])
  |> list.map(glearray.from_list)
}

fn print_matrix(m: Matrix(Int)) {
  io.println(
    m
    |> glearray.to_list()
    |> list.map(fn(row) {
      row
      |> glearray.to_list()
      |> list.map(int.to_string)
      |> string.join(" ")
    })
    |> string.join("\n"),
  )
}

fn has_xmas(tiles: List(Int)) -> Bool {
  case tiles {
    [1, 2, 3, 4] -> True
    _ -> False
  }
}

fn format_row(row: Array(Int)) -> String {
  row
  |> glearray.iterate()
  |> iterator.map(fn(x) {
    case x {
      1 -> "X"
      2 -> "M"
      3 -> "A"
      4 -> "S"
      _ -> "."
    }
  })
  |> iterator.to_list()
  |> string.join("")
}

fn row_count_xmas(row: Array(Int)) {
  let count =
    row
    |> glearray.to_list()
    |> list.window(4)
    |> list.count(has_xmas)

  // io.println(format_row(row) <> "\t" <> int.to_string(count))

  count
}

fn array_reverse(a: Array(Int)) -> Array(Int) {
  a
  |> glearray.to_list()
  |> list.reverse()
  |> glearray.from_list()
}

pub fn count_xmas(m: Matrix(Int)) -> Int {
  let diagonals = diagonals(m)
  let rows =
    m
    |> glearray.to_list()
  let cols =
    m
    |> matrix_transpose()
    |> glearray.to_list()
  [
    diagonals,
    rows,
    rows
      |> list.map(array_reverse),
    cols,
    cols
      |> list.map(array_reverse),
  ]
  |> list.flatten()
  |> list.map(row_count_xmas)
  |> list.fold(0, int.add)
}

fn mas_down_right(m: Matrix(Int), row: Int, col: Int) -> List(#(Int, Int)) {
  list.range(-1, 1)
  |> list.map(fn(i) { #(row + i, col + i) })
}

fn mas_down_left(m: Matrix(Int), row: Int, col: Int) -> List(#(Int, Int)) {
  list.range(-1, 1)
  |> list.map(fn(i) { #(row + i, col - i) })
}

fn mas_horizontal(m: Matrix(Int), row: Int, col: Int) -> List(#(Int, Int)) {
  list.range(-1, 1)
  |> list.map(fn(i) { #(row, col + i) })
}

fn mas_vertical(m: Matrix(Int), row: Int, col: Int) -> List(#(Int, Int)) {
  list.range(-1, 1)
  |> list.map(fn(i) { #(row + i, col) })
}

fn matrix_get_many(m: Matrix(Int), at: List(#(Int, Int))) -> List(Int) {
  at
  |> list.map(fn(ij) {
    let #(i, j) = ij
    let assert Ok(x) = matrix_get(m, i, j)
    x
  })
}

fn mas_match(ls: List(Int)) -> Bool {
  case ls {
    [2, 3, 4] | [4, 3, 2] -> True
    _ -> False
  }
}

pub fn check_mas_x(m: Matrix(Int), row: Int, col: Int) -> Bool {
  let down_right = matrix_get_many(m, mas_down_right(m, row, col))
  let down_left = matrix_get_many(m, mas_down_left(m, row, col))
  mas_match(down_right) && mas_match(down_left)
}

pub fn count_mas_x(m: Matrix(Int)) -> Int {
  let #(rows, cols) = matrix_shape(m)
  yielder.range(1, rows - 2)
  |> yielder.map(fn(i) {
    yielder.range(1, cols - 2)
    |> yielder.filter(fn(j) { check_mas_x(m, i, j) })
    |> yielder.length()
  })
  |> yielder.fold(0, int.add)
}

pub fn main() {
  let input = read_input("inputs/day04.input")
  io.println("XMAS Count: " <> int.to_string(count_xmas(input)))
  io.println("MAS X Count: " <> int.to_string(count_mas_x(input)))
}
