import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/regexp.{Match}
import gleam/result
import gleam/string
import simplifile

fn parse_number(s: String) -> Int {
  let assert Ok(n) = int.base_parse(s, 10)
  n
}

fn read_input(input: String) -> String {
  let assert Ok(contents) = simplifile.read(input)
  contents
}

pub type Instruction {
  Mul(a: Int, b: Int)
  Do
  Dont
}

pub fn extract_instructions(input: String) -> List(Instruction) {
  let assert Ok(re) =
    regexp.from_string("do\\(\\)|don't\\(\\)|mul\\(([0-9]+),([0-9]+)\\)")
  io.println("")
  regexp.scan(with: re, content: input)
  |> list.map(fn(match) {
    case match {
      Match(s, _) -> io.println(s)
    }

    case match {
      Match(_, [Some(a), Some(b)]) -> Mul(parse_number(a), parse_number(b))
      Match("do()", _) -> Do
      Match("don't()", _) -> Dont
      _ -> panic
    }
  })
}

pub fn eval_aux(instrs: List(Instruction), do: Bool, acc: Int) -> Int {
  case instrs {
    [] -> acc
    [first, ..rest] ->
      case first {
        Do -> eval_aux(rest, True, acc)
        Dont -> eval_aux(rest, False, acc)
        Mul(a, b) if do -> eval_aux(rest, do, acc + a * b)
        _ -> eval_aux(rest, do, acc)
      }
  }
}

pub fn eval_input(input: String) -> Int {
  extract_instructions(input)
  |> eval_aux(True, 0)
}

pub fn main() {
  let input = read_input("inputs/day03.input")
  io.println("Result: " <> int.to_string(eval_input(input)))
}
