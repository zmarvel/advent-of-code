import day01
import day02
import day03.{Do, Dont, Mul}
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
  |> should.equal([1, 2, 3, 4])
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

pub fn day03_test() {
  day03.extract_instructions("mul(44,46)")
  |> should.equal([Mul(44, 46)])
  day03.extract_instructions("mul(44,46) mul(123,4)mul(123,4)")
  |> should.equal([Mul(44, 46), Mul(123, 4), Mul(123, 4)])
  day03.extract_instructions(
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))",
  )
  |> should.equal([Mul(2, 4), Mul(5, 5), Mul(11, 8), Mul(8, 5)])
  day03.extract_instructions(
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))",
  )
  |> should.equal([Mul(2, 4), Dont, Mul(5, 5), Mul(11, 8), Do, Mul(8, 5)])

  day03.eval_input(
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))",
  )
  |> should.equal(161)
  day03.eval_input(
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))",
  )
  |> should.equal(48)
}
