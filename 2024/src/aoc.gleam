import argv
import day01
import day02

pub fn main() {
  case argv.load().arguments {
    ["day01"] -> day01.main()
    ["day02"] -> day02.main()
    _ -> panic as "invalid argument"
  }
}
