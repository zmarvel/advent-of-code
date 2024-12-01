import gleeunit
import gleeunit/should
import aoc

pub fn main() {
  gleeunit.main()
}

pub fn day01_test() {
  let #(left, right) = aoc.read_lists("inputs/day01.test")
  aoc.find_total_distance(left, right)
  |> should.equal(11)

  aoc.find_similarity_score(left, right)
  |> should.equal(31)
}
