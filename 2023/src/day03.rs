use std::char;

fn is_symbol(c: char) -> bool {
    return c == '*';
}

fn find_numbers(line: &str) -> Vec<(usize, usize)> {
    let ascii_line: Vec<char> = line.chars().collect();
    let mut numbers = vec![];
    let mut number_start = None;
    for (i, c) in ascii_line.iter().enumerate() {
        if !c.is_numeric() && !number_start.is_none() {
            numbers.push((number_start.unwrap(), i));
            number_start = None;
        }
        if c.is_numeric() && number_start.is_none() {
            number_start = Some(i)
        }
    }
    if !number_start.is_none() {
        numbers.push((number_start.unwrap(), ascii_line.len()));
    }

    numbers
}

fn find_symbols(line: &str) -> Vec<usize> {
    let ascii_line: Vec<char> = line.chars().collect();
    ascii_line
        .iter()
        .enumerate()
        .filter(|(_i, c)| is_symbol(**c))
        .map(|(i, _c)| i)
        .collect()
}

fn is_connected(gear_pos: usize, number_pos: (usize, usize)) -> bool {
    let (num_start, num_end) = number_pos;
    let min_neighbor = if num_start == 0usize {
        0
    } else {
        num_start - 1
    };
    let max_neighbor = num_end + 1;
    gear_pos >= min_neighbor && gear_pos < max_neighbor
}

fn find_vertical_gears(
    prev_numbers: &Vec<(usize, usize)>,
    curr_symbols: &Vec<usize>,
    next_numbers: &Vec<(usize, usize)>,
) -> i64 {
    curr_symbols
        .iter()
        .filter(|symbol_pos| {
            prev_numbers
                .iter()
                .any(|prev_pos| is_connected(**symbol_pos, *prev_pos))
                && next_numbers
                    .iter()
                    .any(|next_pos| is_connected(**symbol_pos, *next_pos))
        })
        .fold(0, |acc, x| acc + (*x as i64))
}

fn find_adjacent_numbers(symbol_pos: usize, numbers: &Vec<(usize, usize)>) -> Vec<(usize, usize)> {
    numbers
        .iter()
        .filter(|(num_start, num_end)| {
            let min_neighbor = if *num_start == 0usize {
                0
            } else {
                num_start - 1
            };
            let max_neighbor = num_end + 1;
            symbol_pos >= min_neighbor && symbol_pos < max_neighbor
        })
        .map(|(num_start, num_end)| (*num_start, *num_end))
        .collect()
}

fn count_adjacent_numbers(symbol_pos: usize, numbers: &Vec<(usize, usize)>) -> usize {
    find_adjacent_numbers(symbol_pos, numbers).len()
}

fn parse_i64_substr(line: &str, num_pos: (usize, usize)) -> i64 {
    let (num_start, num_end) = num_pos;
    if num_start == num_end {
        panic!("num_start == num_end");
    }
    let substr = line.get(num_start..num_end).unwrap();
    // println!("{},{}: \"{}\"", num_start, num_end, substr);
    substr.parse::<i64>().unwrap()
}

fn parse_neighbor_numbers(line: &str, number_positions: Vec<(usize, usize)>) -> Vec<i64> {
    number_positions
        .iter()
        .map(|&num_pos| parse_i64_substr(line, num_pos))
        .collect()
}

pub fn scan_line(prev_line: &str, line: &str, next_line: &str) -> i64 {
    let ascii_line: Vec<char> = line.chars().collect();
    if prev_line.len() != ascii_line.len() || next_line.len() != ascii_line.len() {
        panic!("Invalid len");
    }

    let curr_symbols = find_symbols(line);
    let prev_numbers = find_numbers(prev_line);
    let curr_numbers = find_numbers(line);
    let next_numbers = find_numbers(next_line);

    let gears = curr_symbols
        .iter()
        .filter(|&&sym| {
            count_adjacent_numbers(sym, &prev_numbers)
                + count_adjacent_numbers(sym, &curr_numbers)
                + count_adjacent_numbers(sym, &next_numbers)
                == 2
        })
        .map(|&sym| {
            let prev_neighbors =
                parse_neighbor_numbers(prev_line, find_adjacent_numbers(sym, &prev_numbers));
            let mut curr_neighbors =
                parse_neighbor_numbers(line, find_adjacent_numbers(sym, &curr_numbers));
            let mut next_neighbors =
                parse_neighbor_numbers(next_line, find_adjacent_numbers(sym, &next_numbers));
            let mut neighbors = Vec::from(prev_neighbors.as_slice());
            neighbors.append(&mut curr_neighbors);
            neighbors.append(&mut next_neighbors);

            let x = neighbors.get(0).unwrap();
            let y = neighbors.get(1).unwrap();
            x * y
        });

    gears.fold(0, |acc, x| acc + x)
}

#[cfg(test)]
mod tests {
    use crate::day03::{parse_i64_substr, scan_line};

    #[test]
    fn parse_i64_substr_success() {
        let line = "..123";
        assert_eq!(parse_i64_substr(&line, (2, 5)), 123);
    }

    #[test]
    fn scan_line_no_gears() {
        let prev_line = String::from("..123");
        let curr_line = String::from("..#..");
        let next_line = String::from("..123");
        assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 0);
    }

    #[test]
    fn scan_line_vertical() {
        {
            let prev_line = String::from("..12.");
            let curr_line = String::from(".*...");
            let next_line = String::from("..12.");
            assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 144);
        }
        {
            let prev_line = String::from("..12.");
            let curr_line = String::from("....*");
            let next_line = String::from("..12.");
            assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 144);
        }
    }

    #[test]
    fn scan_line_horizontal() {
        {
            let prev_line = String::from(".....");
            let curr_line = String::from("12*12");
            let next_line = String::from(".....");
            assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 144);
        }
        {
            let prev_line = String::from("12.12");
            let curr_line = String::from("..*..");
            let next_line = String::from(".....");
            assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 144);
        }
        {
            let prev_line = String::from(".....");
            let curr_line = String::from("..*..");
            let next_line = String::from("12.12");
            assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 144);
        }
    }

    #[test]
    fn scan_line_diagonal() {
        {
            let prev_line = String::from("12...");
            let curr_line = String::from("..*..");
            let next_line = String::from("...12");
            assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 144);
        }
        {
            let prev_line = String::from("...12");
            let curr_line = String::from("..*..");
            let next_line = String::from("12...");
            assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 144);
        }
        {
            let prev_line = String::from(".....");
            let curr_line = String::from("12*..");
            let next_line = String::from("...12");
            assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 144);
        }
        {
            let prev_line = String::from(".....");
            let curr_line = String::from("..*12");
            let next_line = String::from("12...");
            assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 144);
        }
        {
            let prev_line = String::from("12...");
            let curr_line = String::from("..*12");
            let next_line = String::from(".....");
            assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 144);
        }
        {
            let prev_line = String::from("...12");
            let curr_line = String::from("12*..");
            let next_line = String::from(".....");
            assert_eq!(scan_line(&prev_line, &curr_line, &next_line), 144);
        }
    }
}
