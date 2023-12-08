mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;

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

fn day05() {
    if let Ok(lines) = read_lines(Path::new("day05.input")) {
        let lines_strings: Vec<String> = lines.map(|line| line.unwrap()).collect();

        let seeds = day05::read_seeds(&lines_strings[0]);
        let maps = day05::read_maps(&lines_strings);
        let result = day05::map_seeds(&seeds, &maps);
        println!("result {}", result);
    } else {
        println!("Failed to open input")
    }
}

fn day06() {
    if let Ok(lines) = read_lines(Path::new("day06.input")) {
        let lines_strings: Vec<String> = lines.map(|line| line.unwrap()).collect();
        let result = day06::do_part2(lines_strings.as_slice());
        println!("result {}", result);
    } else {
        println!("Failed to open input")
    }
}

fn day07() {
    if let Ok(lines) = read_lines(Path::new("day07.input")) {
        let lines_strings: Vec<String> = lines.map(|line| line.unwrap()).collect();
        let result = day07::do_part1(
            lines_strings
                .iter()
                .map(String::as_ref)
                .collect::<Vec<&str>>()
                .as_slice(),
        );
        println!("result {}", result);
    } else {
        println!("Failed to open input")
    }
}

fn day08() {
    if let Ok(lines) = read_lines(Path::new("day08.input")) {
        let lines_strings: Vec<String> = lines.map(|line| line.unwrap()).collect();
        let result = day08::do_part2(
            lines_strings
                .iter()
                .map(String::as_ref)
                .collect::<Vec<&str>>()
                .as_slice(),
        );
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
    } else if arg == "day05" {
        day05();
    } else if arg == "day06" {
        day06();
    } else if arg == "day07" {
        day07();
    } else if arg == "day08" {
        day08();
    }
}
