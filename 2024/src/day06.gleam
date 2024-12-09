import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import simplifile

pub fn find_obstacles(lines: List(String)) -> List(#(Int, Int)) {
  lines
  |> list.index_fold([], fn(acc, line, i) {
    string.to_graphemes(line)
    |> list.index_fold(acc, fn(acc, c, j) {
      case c {
        "#" -> [#(i, j), ..acc]
        _ -> acc
      }
    })
  })
  // reverse just to make testing easier
  |> list.reverse
}

pub type Direction {
  Up
  Down
  Left
  Right
}

fn direction(s: String) -> Result(Direction, Nil) {
  case s {
    "<" -> Ok(Left)
    "^" -> Ok(Up)
    ">" -> Ok(Right)
    "v" -> Ok(Down)
    _ -> Error(Nil)
  }
}

fn next_direction(d: Direction) -> Direction {
  case d {
    Left -> Up
    Up -> Right
    Right -> Down
    Down -> Left
  }
}

pub fn find_guard(lines: List(String)) -> Result(#(Direction, #(Int, Int)), Nil) {
  lines
  |> list.index_fold(Error(Nil), fn(acc, line, i) {
    string.to_graphemes(line)
    |> list.index_fold(acc, fn(acc, c, j) {
      case acc {
        Ok(_) -> acc
        _ ->
          direction(c)
          |> result.map(fn(direction) { #(direction, #(i, j)) })
      }
    })
  })
}

fn find_obstacle_up(
  obstacles: List(#(Int, Int)),
  shape: #(Int, Int),
  i: Int,
  j: Int,
) -> Result(#(Int, Int), #(Int, Int)) {
  // if direction is Up, check obstacles with col == j and row < i
  obstacles
  |> list.filter(fn(p) {
    let #(row, col) = p
    row < i && col == j
  })
  |> list.sort(fn(a, b) {
    // put the closest obstacle first
    let #(arow, _) = a
    let #(brow, _) = b
    int.compare(i - arow, i - brow)
  })
  |> list.first
  |> result.map_error(fn(_) { #(0, j) })
}

fn find_obstacle_down(
  obstacles: List(#(Int, Int)),
  shape: #(Int, Int),
  i: Int,
  j: Int,
) -> Result(#(Int, Int), #(Int, Int)) {
  let #(nrows, _) = shape
  // if direction is Down, check for obstacles with col == j and row > i
  obstacles
  |> list.filter(fn(p) {
    let #(row, col) = p
    row > i && col == j
  })
  |> list.sort(fn(a, b) {
    // put the closest obstacle first
    let #(arow, _) = a
    let #(brow, _) = b
    int.compare(arow - i, brow - i)
  })
  |> list.first
  |> result.map_error(fn(_) { #(nrows - 1, j) })
}

fn find_obstacle_right(
  obstacles: List(#(Int, Int)),
  shape: #(Int, Int),
  i: Int,
  j: Int,
) -> Result(#(Int, Int), #(Int, Int)) {
  let #(_, ncols) = shape
  // if direction is Right, check for obstacles with row == i and col > j
  obstacles
  |> list.filter(fn(p) {
    let #(row, col) = p
    row == i && col > j
  })
  |> list.sort(fn(a, b) {
    // put the closest obstacle first
    let #(_, acol) = a
    let #(_, bcol) = b
    int.compare(acol - j, bcol - j)
  })
  |> list.first
  |> result.map_error(fn(_) { #(i, ncols - 1) })
}

fn find_obstacle_left(
  obstacles: List(#(Int, Int)),
  shape: #(Int, Int),
  i: Int,
  j: Int,
) -> Result(#(Int, Int), #(Int, Int)) {
  // if direction is Left, check for obstacles with row == i and col < j
  obstacles
  |> list.filter(fn(p) {
    let #(row, col) = p
    row == i && col < j
  })
  |> list.sort(fn(a, b) {
    // put the closest obstacle first
    let #(_, acol) = a
    let #(_, bcol) = b
    int.compare(j - acol, j - bcol)
  })
  |> list.first
  |> result.map_error(fn(_) { #(i, 0) })
}

fn fmt_tuple(tup: #(Int, Int)) -> String {
  let #(a, b) = tup
  "(" <> int.to_string(a) <> "," <> int.to_string(b) <> ")"
}

fn between(a: #(Int, Int), b: #(Int, Int)) -> List(#(Int, Int)) {
  case a, b {
    #(ai, aj), #(bi, bj) if ai < bi && aj == bj ->
      list.range(ai, bi)
      |> list.map(fn(i) { #(i, aj) })
    #(ai, aj), #(bi, bj) if ai > bi && aj == bj ->
      list.range(bi, ai)
      |> list.map(fn(i) { #(i, aj) })
    #(ai, aj), #(bi, bj) if ai == bi && aj < bj ->
      list.range(aj, bj)
      |> list.map(fn(j) { #(ai, j) })
    #(ai, aj), #(bi, bj) if ai == bi && aj > bj ->
      list.range(bj, aj)
      |> list.map(fn(j) { #(ai, j) })
    #(ai, aj), #(bi, bj) if ai == bi && aj == bj -> [#(ai, aj)]
    _, _ -> {
      io.println("INVALID " <> fmt_tuple(a) <> " " <> fmt_tuple(b))
      []
    }
  }
}

fn visit(
  visited: Set(#(Int, Int)),
  curr: #(Int, Int),
  next: #(Int, Int),
) -> Set(#(Int, Int)) {
  let path =
    between(curr, next)
    |> set.from_list
  let len = set.size(path)
  io.println(
    "visit "
    <> fmt_tuple(curr)
    <> " -> "
    <> fmt_tuple(next)
    <> " len: "
    <> int.to_string(len),
  )

  path
  |> set.union(visited)
}

fn result_add_direction(
  r: Result(a, b),
  d: Direction,
) -> Result(#(Direction, a), #(Direction, b)) {
  r
  |> result.map(fn(x) { #(d, x) })
  |> result.map_error(fn(e) { #(d, e) })
}

fn step_guard_aux(
  guard: #(Direction, #(Int, Int)),
  obstacles: List(#(Int, Int)),
  shape: #(Int, Int),
  visited: Set(#(Int, Int)),
) -> Set(#(Int, Int)) {
  let #(nrows, ncols) = shape
  let #(direction, #(i, j)) = guard
  let next_result = case direction {
    Up ->
      find_obstacle_up(obstacles, shape, i, j)
      |> result.map(fn(obstacle) {
        let #(row, col) = obstacle
        #(row + 1, col)
      })
      |> result_add_direction(Right)
    Right ->
      find_obstacle_right(obstacles, shape, i, j)
      |> result.map(fn(obstacle) {
        let #(row, col) = obstacle
        #(row, col - 1)
      })
      |> result_add_direction(Down)
    Down ->
      find_obstacle_down(obstacles, shape, i, j)
      |> result.map(fn(obstacle) {
        let #(row, col) = obstacle
        #(row - 1, col)
      })
      |> result_add_direction(Left)
    Left ->
      find_obstacle_left(obstacles, shape, i, j)
      |> result.map(fn(obstacle) {
        let #(row, col) = obstacle
        #(row, col + 1)
      })
      |> result_add_direction(Up)
  }
  case next_result {
    Ok(#(next_dir, next)) ->
      step_guard_aux(
        #(next_dir, next),
        obstacles,
        shape,
        visit(visited, #(i, j), next),
      )
    Error(#(next_dir, next)) -> visit(visited, #(i, j), next)
  }
}

pub fn step_guard(board: List(String)) -> Int {
  io.println("")
  let assert Ok(guard) = find_guard(board)
  let obstacles = find_obstacles(board)
  let assert Ok(row) = list.first(board)
  let shape = #(list.length(board), string.length(row))
  io.println("shape: " <> fmt_tuple(shape))
  step_guard_aux(guard, obstacles, shape, set.new())
  |> set.size
}

fn read_input(path: String) -> List(String) {
  let assert Ok(contents) = simplifile.read(path)
  let nonempty = fn(s) { s != "" }
  let lines =
    contents
    |> string.split("\n")
    |> list.filter(nonempty)
}

pub fn main() {
  let board = read_input("inputs/day06.input")
  io.println("steps: " <> int.to_string(step_guard(board)))
}
