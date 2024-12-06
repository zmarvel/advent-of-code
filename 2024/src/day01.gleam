import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import simplifile

pub fn find_total_distance(left: List(Int), right: List(Int)) -> Int {
  let left_sorted = list.sort(left, int.compare)
  let right_sorted = list.sort(right, int.compare)
  let distances =
    list.map2(left_sorted, right_sorted, fn(a, b) { int.absolute_value(a - b) })
  list.fold(distances, 0, int.add)
}

fn parse_number(s: String) -> Int {
  let assert Ok(n) = int.base_parse(s, 10)
  n
}

pub fn read_lists(input: String) -> #(List(Int), List(Int)) {
  let assert Ok(contents) = simplifile.read(input)
  contents
  |> string.split("\n")
  |> list.filter(fn(line) { line != "" })
  |> list.map(fn(line) {
    let assert Ok(#(left, right)) = string.split_once(line, "   ")
    #(parse_number(left), parse_number(right))
  })
  |> list.unzip
}

pub fn find_similarity_score(left: List(Int), right: List(Int)) -> Int {
  let increment = fn(x) {
    case x {
      Some(i) -> i + 1
      None -> 1
    }
  }
  let right_counts =
    right
    |> list.fold(dict.new(), fn(acc, x) { dict.upsert(acc, x, increment) })
  let assert Ok(similarity_score) =
    left
    |> list.map(fn(x) {
      let y = case dict.get(right_counts, x) {
        Ok(n) -> n
        _ -> 0
      }
      x * y
    })
    |> list.reduce(int.add)
  similarity_score
}

pub fn main() {
  let #(left, right) = read_lists("inputs/day01.input")
  io.println(
    "Total distance: "
    <> {
      find_total_distance(left, right)
      |> int.to_string
    },
  )
  io.println(
    "Similarity score: "
    <> {
      find_similarity_score(left, right)
      |> int.to_string
    },
  )
}
