mod day01;
mod day02;
mod day03;
mod day04;

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
        let mut result = 0;
        for line in lines {
            let power = day02::parse_line(&line.unwrap());
            result += power
        }
        println!("{}", result)
    } else {
        println!("Failed to open input")
    }
}

fn day03() {
    if let Ok(lines) = read_lines(Path::new("day03.input")) {
        let lines_vec: Vec<String> = lines.map(|line| line.unwrap()).collect();
        let empty_line = ".".repeat(lines_vec[0].len());
        let mut result = 0;
        for i in 0..lines_vec.len() {
            let prev_line = if i == 0 {
                &empty_line
            } else {
                &lines_vec[i - 1]
            };
            let curr_line = &lines_vec[i];
            let next_line = if i == lines_vec.len() - 1 {
                &empty_line
            } else {
                &lines_vec[i + 1]
            };
            result += day03::scan_line(&prev_line, &curr_line, &next_line);
        }
        println!("result {}", result);
    } else {
        println!("Failed to open input")
    }
}

fn day04() {
    if let Ok(lines) = read_lines(Path::new("day04.input")) {
        let lines_strings: Vec<String> = lines.map(|line| line.unwrap()).collect();
        let lines_slices: Vec<&str> = lines_strings.iter().map(|line| line.as_str()).collect();
        let result = day04::count_total_scorecards(&lines_slices);
        println!("result {}", result);
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
    } else if arg == "day03" {
        day03();
    } else if arg == "day04" {
        day04();
    }
}
