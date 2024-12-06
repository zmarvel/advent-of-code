import argv
import day01
import day02
import day03

pub fn main() {
  case argv.load().arguments {
    ["day01"] -> day01.main()
    ["day02"] -> day02.main()
    ["day03"] -> day03.main()
    _ -> panic as "invalid argument"
  }
}
