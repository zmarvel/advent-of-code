import day01
import day02
import day03.{Do, Dont, Mul}
import day04
import day05
import gleam/list
import glearray
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

pub fn day04_test() {
  day04.to_matrix(["BCDE", "XMAS"])
  |> day04.matrix_to_list()
  |> should.equal([[0, 0, 0, 0], [1, 2, 3, 4]])

  let m = day04.to_matrix(["AXXX", "MMAS"])
  m
  |> day04.matrix_to_list()
  |> should.equal([[3, 1, 1, 1], [2, 2, 3, 4]])

  m
  |> day04.matrix_transpose()
  |> day04.matrix_to_list()
  |> should.equal([[3, 2], [1, 2], [1, 3], [1, 4]])

  day04.mirror_horizontal(m)
  |> day04.matrix_to_list()
  |> should.equal([[2, 2, 3, 4], [3, 1, 1, 1]])

  day04.mirror_vertical(m)
  |> day04.matrix_to_list()
  |> should.equal([[1, 1, 1, 3], [4, 3, 2, 2]])

  day04.to_matrix(["..X...", ".SAMX.", ".A..A.", "XMAS.S", ".X...."])
  |> day04.count_xmas()
  |> should.equal(4)

  day04.to_matrix([
    "MMMSXXMASM", "MSAMXMSMSA", "AMXSXMAAMM", "MSAMASMSMX", "XMASAMXAMM",
    "XXAMMXXAMA", "SMSMSASXSS", "SAXAMASAAA", "MAMMMXMMMM", "MXMXAXMASX",
  ])
  |> day04.count_xmas()
  |> should.equal(18)
}

pub fn day04_diagonals_test() {
  let m = day04.to_matrix(["AXXX", "MMAS"])
  day04.diagonals(m)
  |> list.map(glearray.to_list)
  |> should.equal([
    [3, 2],
    [2],
    [1, 3],
    [1, 4],
    [1],
    [1, 3],
    [4],
    [1, 2],
    [1, 2],
    [3],
    [3, 1],
    [4],
    [2, 1],
    [2, 1],
    [3],
    [2, 3],
    [2],
    [3, 1],
    [4, 1],
    [1],
  ])
}

pub fn day04_count_mas_x_test() {
  day04.to_matrix(["M.S", ".A.", "M.S"])
  |> day04.count_mas_x()
  |> should.equal(1)

  day04.to_matrix([
    "MMMSXXMASM", "MSAMXMSMSA", "AMXSXMAAMM", "MSAMASMSMX", "XMASAMXAMM",
    "XXAMMXXAMA", "SMSMSASXSS", "SAXAMASAAA", "MAMMMXMMMM", "MXMXAXMASX",
  ])
  |> day04.count_mas_x()
  |> should.equal(9)
}

pub fn day05_check_update_test() {
  let rules = [#(61, 13), #(29, 13), #(61, 29)]
  day05.check_update([61, 13, 29], day05.index_rules(rules))
  |> should.equal(False)

  let rules = [
    #(47, 53),
    #(97, 61),
    #(97, 47),
    #(75, 29),
    #(61, 13),
    #(75, 53),
    #(29, 13),
    #(97, 29),
    #(53, 29),
    #(61, 53),
    #(97, 53),
    #(61, 29),
    #(47, 13),
    #(75, 47),
    #(97, 75),
    #(47, 61),
    #(75, 61),
    #(47, 29),
    #(75, 13),
    #(53, 13),
  ]
  day05.check_update([75, 47, 61, 53, 29], day05.index_rules(rules))
  |> should.equal(True)
}

pub fn day05_filter_updates_test() {
  let rules = [
    #(47, 53),
    #(97, 13),
    #(97, 61),
    #(97, 47),
    #(75, 29),
    #(61, 13),
    #(75, 53),
    #(29, 13),
    #(97, 29),
    #(53, 29),
    #(61, 53),
    #(97, 53),
    #(61, 29),
    #(47, 13),
    #(75, 47),
    #(97, 75),
    #(47, 61),
    #(75, 61),
    #(47, 29),
    #(75, 13),
    #(53, 13),
  ]
  let updates = [
    [75, 47, 61, 53, 29],
    [97, 61, 53, 29, 13],
    [75, 29, 13],
    [75, 97, 47, 61, 53],
    [61, 13, 29],
    [97, 13, 75, 29, 47],
  ]
  day05.filter_updates(rules, updates)
  |> should.equal([[75, 47, 61, 53, 29], [97, 61, 53, 29, 13], [75, 29, 13]])
}

pub fn day05_sum_middle_pages_test() {
  let rules = [
    #(47, 53),
    #(97, 13),
    #(97, 61),
    #(97, 47),
    #(75, 29),
    #(61, 13),
    #(75, 53),
    #(29, 13),
    #(97, 29),
    #(53, 29),
    #(61, 53),
    #(97, 53),
    #(61, 29),
    #(47, 13),
    #(75, 47),
    #(97, 75),
    #(47, 61),
    #(75, 61),
    #(47, 29),
    #(75, 13),
    #(53, 13),
  ]
  let updates = [
    [75, 47, 61, 53, 29],
    [97, 61, 53, 29, 13],
    [75, 29, 13],
    [75, 97, 47, 61, 53],
    [61, 13, 29],
    [97, 13, 75, 29, 47],
  ]
  day05.filter_updates(rules, updates)
  |> day05.sum_middle_pages
  |> should.equal(143)
}

pub fn day05_toposort_test() {
  day05.toposort([#(61, 13), #(29, 13), #(61, 29)])
  |> should.equal([61, 29, 13])
}

pub fn day05_resort_update_test() {
  let rules = [
    #(47, 53),
    #(97, 13),
    #(97, 61),
    #(97, 47),
    #(75, 29),
    #(61, 13),
    #(75, 53),
    #(29, 13),
    #(97, 29),
    #(53, 29),
    #(61, 53),
    #(97, 53),
    #(61, 29),
    #(47, 13),
    #(75, 47),
    #(97, 75),
    #(47, 61),
    #(75, 61),
    #(47, 29),
    #(75, 13),
    #(53, 13),
  ]
  day05.resort_update(rules, [61, 13, 29])
  |> should.equal([61, 29, 13])

  day05.resort_update(rules, [75, 97, 47, 61, 53])
  |> should.equal([97, 75, 47, 61, 53])
  day05.resort_update(rules, [61, 13, 29])
  |> should.equal([61, 29, 13])
  day05.resort_update(rules, [97, 13, 75, 29, 47])
  |> should.equal([97, 75, 47, 29, 13])
}
