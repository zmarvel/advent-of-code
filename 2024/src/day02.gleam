import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

fn parse_number(s: String) -> Int {
  let assert Ok(n) = int.base_parse(s, 10)
  n
}

pub fn read_reports(input: String) -> List(List(Int)) {
  let assert Ok(contents) = simplifile.read(input)
  contents
  |> string.split("\n")
  |> list.filter(fn(line) { line != "" })
  |> list.map(fn(line) {
    string.split(line, " ")
    |> list.map(parse_number)
  })
}

fn is_tup_decreasing(tup: #(Int, Int)) -> Bool {
  let #(a, b) = tup
  b <= a
}

fn is_decreasing(report: List(Int)) -> Bool {
  report
  |> list.window_by_2()
  |> list.fold(True, fn(acc, tup) { acc && is_tup_decreasing(tup) })
}

fn is_tup_increasing(tup: #(Int, Int)) -> Bool {
  let #(a, b) = tup
  a <= b
}

fn is_increasing(report: List(Int)) -> Bool {
  report
  |> list.window_by_2()
  |> list.fold(True, fn(acc, tup) { acc && is_tup_increasing(tup) })
}

fn is_delta_safe(x) {
  let d = int.absolute_value(x)
  d >= 1 && d <= 3
}

fn is_distance_safe(tup) {
  let #(a, b) = tup
  is_delta_safe(a - b)
}

fn check_adjacent_differences(report: List(Int)) -> Bool {
  report
  |> list.window_by_2()
  |> list.fold(True, fn(acc, tup) { acc && is_distance_safe(tup) })
}

fn fmt_report(report: List(Int)) -> String {
  report
  |> list.map(int.to_string)
  |> string.join(" ")
}

fn is_report_safe(report: List(Int)) -> Bool {
  case report {
    [] -> False
    _ ->
      { is_increasing(report) || is_decreasing(report) }
      && check_adjacent_differences(report)
  }
}

pub fn count_safe_reports(reports: List(List(Int))) -> Int {
  reports
  |> list.count(is_report_safe)
}

fn unindex_report(i_report: List(#(Int, Int))) -> List(Int) {
  i_report
  |> list.map(fn(tup) {
    let #(i, x) = tup
    x
  })
}

pub fn dampen_report(report: List(Int)) -> List(Int) {
  case is_report_safe(report) {
    True -> report
    _ -> {
      let i_report =
        report
        |> list.index_map(fn(x, i) { #(i, x) })

      i_report
      |> list.fold([], fn(acc, tup) {
        case acc {
          [] -> {
            let #(i, x) = tup
            let assert Ok(#(_, i_removed)) =
              list.pop(i_report, fn(tup) {
                let #(j, x) = tup
                i == j
              })
            let removed = unindex_report(i_removed)
            case is_report_safe(removed) {
              True -> removed
              False -> []
              // keep looking
            }
          }
          _ -> acc
        }
      })
    }
  }
}

pub fn count_dampened_safe_reports(reports: List(List(Int))) -> Int {
  reports
  |> list.map(dampen_report)
  |> list.count(is_report_safe)
}

// 328 too low
pub fn main() {
  let reports = read_reports("inputs/day02.input")
  io.println(string.join(
    [
      "Safe reports: "
        <> {
        count_safe_reports(reports)
        |> int.to_string
      },
      "Dampened safe reports: "
        <> {
        count_dampened_safe_reports(reports)
        |> int.to_string
      },
    ],
    "\n",
  ))
}
