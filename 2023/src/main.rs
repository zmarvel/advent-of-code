mod day01;

use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn read_lines(file_path: &Path) -> io::Result<io::Lines<io::BufReader<File>>> {
    let file = File::open(file_path)?;
    Ok(io::BufReader::new(file).lines())
}

fn day01() {
    if let Ok(lines) = read_lines(Path::new("day01.input")) {
        let mut result = 0;
        for line in lines {
            result += day01::extract_calibration_value(&line.unwrap());
        }
        println!("{}", result)
    } else {
        println!("Failed to open input")
    }
}

fn main() {
    day01();
}
