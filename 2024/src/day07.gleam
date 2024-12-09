import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleam/yielder
import simplifile

fn parse_number(s: String) -> Int {
  let assert Ok(n) = int.base_parse(s, 10)
  n
}

fn read_lines(path: String) -> List(String) {
  let assert Ok(contents) = simplifile.read(path)
  let nonempty = fn(s) { s != "" }
  let lines =
    contents
    |> string.split("\n")
    |> list.filter(nonempty)
}

type Equation =
  #(Int, List(Int))

fn parse_line(line: String) -> Equation {
  let assert Ok(#(test_value, numbers)) =
    line
    |> string.split_once(": ")
  let test_value = parse_number(test_value)
  let numbers =
    numbers
    |> string.split(" ")
    |> list.map(parse_number)
  #(test_value, numbers)
}

pub fn parse_input(lines: List(String)) -> List(Equation) {
  lines
  |> list.map(parse_line)
}

pub type Operator {
  Add
  Multiply
}

pub fn operators(n: Int) -> List(List(Operator)) {
  case n {
    0 -> []
    1 -> [[Add], [Multiply]]
    n if n > 0 -> {
      operators(n - 1)
      |> list.fold([], fn(acc, operator_list) {
        [[Add, ..operator_list], [Multiply, ..operator_list], ..acc]
      })
    }
    _ -> panic
  }
}

pub fn eval_equation(numbers: List(Int), operators: List(Operator)) -> Int {
  case numbers {
    [] -> panic
    [a] -> a
    [a, b, ..rest] ->
      case operators {
        [] -> panic
        [Add, ..operators] ->{
          //io.println("add(" <> int.to_string(a) <> "," <> int.to_string(b) <>")")
          eval_equation([a + b, ..rest], operators)
        }
        [Multiply, ..operators] -> {
          //io.println("multiply(" <> int.to_string(a) <> "," <> int.to_string(b) <>")")
          eval_equation([a * b, ..rest], operators)
        }
      }
  }
}

/// Returns an equation's test value if it's valid, otherwise 0
pub fn check_equation(e: Equation) -> Int {
  let #(test_value, numbers) = e
  // we could memoize operators(n) to speed this up
  let n = list.length(numbers)
  operators(n - 1)
  |> yielder.from_list
  |> yielder.fold(0, fn(acc, operators) {
    case acc {
      0 ->
        case eval_equation(numbers, operators) {
          result if result == test_value -> test_value
          _ -> 0
        }
      _ -> acc
    }
  })
}

pub fn total_calibration_result(equations: List(Equation)) {
  equations |> list.map(check_equation) |> int.sum
}

pub fn main() {
  let total = read_lines("inputs/day07.input")
  |> parse_input()
  |> total_calibration_result()
  io.println("Total: " <> int.to_string(total))
}
