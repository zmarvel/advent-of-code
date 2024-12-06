import day01
import day02
import gleam/list
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn day01_test() {
  let #(left, right) = day01.read_lists("inputs/day01.test")
  day01.find_total_distance(left, right)
  |> should.equal(11)

  day01.find_similarity_score(left, right)
  |> should.equal(31)
}

pub fn day02_test() {
  let reports = day02.read_reports("inputs/day02.test")
  day02.count_safe_reports(reports)
  |> should.equal(2)

  day02.dampen_report([7, 6, 4, 2, 1])
  |> should.equal([7, 6, 4, 2, 1])
  day02.dampen_report([1, 3, 2, 4, 5])
  |> should.equal([1, 2, 4, 5])
  day02.dampen_report([8, 6, 4, 4, 1])
  |> should.equal([8, 6, 4, 1])
  day02.dampen_report([8, 6, 4, 3, 4])
  |> should.equal([8, 6, 4, 3])
  day02.dampen_report([2, 1, 2, 3, 4])
  |> should.equal([ 1, 2, 3, 4])
  day02.dampen_report([9, 7, 6, 2, 1])
  |> should.equal([])
  day02.dampen_report([65, 68, 71, 72, 71])
  |> should.equal([65, 68, 71, 72])
  day02.dampen_report([36, 38, 41, 42, 45, 49, 47])
  |> should.equal([36, 38, 41, 42, 45, 47])

  day02.count_dampened_safe_reports([[9, 7, 6, 2, 1]])
  |> should.equal(0)
  day02.count_dampened_safe_reports(reports)
  |> should.equal(4)
}
