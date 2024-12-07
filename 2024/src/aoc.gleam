import argv
import day01
import day02
import day03
import day04
import day05

pub fn main() {
  case argv.load().arguments {
    ["day01"] -> day01.main()
    ["day02"] -> day02.main()
    ["day03"] -> day03.main()
    ["day04"] -> day04.main()
    ["day05"] -> day05.main()
    _ -> panic as "invalid argument"
  }
}
