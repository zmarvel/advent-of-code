mod day01;
mod day02;

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

fn day02() {
    if let Ok(lines) = read_lines(Path::new("day02.input")) {
        let requirements = day02::DiceCount {
            red: 12,
            green: 13,
            blue: 14,
        };
        let mut result = 0;
        for line in lines {
            match day02::parse_line(&requirements, &line.unwrap()) {
                Some(count) => result += count,
                None => (),
            }
        }
        println!("{}", result)
    } else {
        println!("Failed to open input")
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let arg = &args[1];
    if arg == "day01" {
        day01();
    } else if arg == "day02" {
        day02();
    }
}
