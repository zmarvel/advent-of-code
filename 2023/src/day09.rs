fn parse_line(line: &str) -> Vec<i64> {
    line.split_whitespace()
        .map(|s| s.parse::<i64>().unwrap())
        .collect()
}

fn diff_line(line: &[i64]) -> Vec<i64> {
    std::iter::zip(line[0..].iter(), line[1..].iter())
        .map(|(x, y)| y - x)
        .collect()
}

// Applies the `diff_line` operation to line until `line` contains only zeros. Returns the final number in each line.
fn diff_line_until_zero_tail(line: &[i64]) -> Vec<i64> {
    let mut result = vec![line[line.len() - 1]];
    let mut line = diff_line(line);
    while line.iter().any(|&x| x != 0) {
        result.push(line[line.len() - 1]);
        line = diff_line(&line);
    }
    result.push(0);
    result
}

// Applies the `diff_line` operation to line until `line` contains only zeros. Returns the first number in each line.
fn diff_line_until_zero_head(line: &[i64]) -> Vec<i64> {
    let mut result = vec![line[0]];
    let mut line = diff_line(line);
    while line.iter().any(|&x| x != 0) {
        println!("{:?}", line);
        result.push(line[0]);
        line = diff_line(&line);
    }
    result.push(0);
    result
}

fn extrapolate(line_end: i64, next_line_end: i64) -> i64 {
    line_end + next_line_end
}

// Extrapolate the end of the first line.
fn extrapolate_lines_tail(line_ends: &[i64], next_line_end: i64) -> i64 {
    if line_ends.len() > 0 {
        extrapolate_lines_tail(&line_ends[1..], extrapolate(line_ends[0], next_line_end))
    } else {
        next_line_end
    }
}

// Extrapolate the beginning of the first line.
fn extrapolate_lines_head(line_heads: &[i64], next_line_head: i64) -> i64 {
    if line_heads.len() > 0 {
        extrapolate_lines_head(&line_heads[1..], line_heads[0] - next_line_head)
    } else {
        next_line_head
    }
}

pub fn do_part1(lines: &[&str]) -> i64 {
    lines
        .iter()
        .map(|line| {
            let line_ends = diff_line_until_zero_tail(parse_line(line).as_slice());
            extrapolate_lines_tail(&line_ends, 0)
        })
        .fold(0, |acc, x| acc + x)
}

pub fn do_part2(lines: &[&str]) -> i64 {
    lines
        .iter()
        .map(|line| {
            let line_rev: Vec<i64> = parse_line(line).iter().rev().map(|&x| x).collect();
            let line_ends = diff_line_until_zero_tail(line_rev.as_slice());
            extrapolate_lines_tail(&line_ends, 0)
        })
        .fold(0, |acc, x| acc + x)
}

#[cfg(test)]
mod tests {
    use crate::day09::{
        diff_line, diff_line_until_zero_head, diff_line_until_zero_tail, do_part1, do_part2,
        extrapolate_lines_head, extrapolate_lines_tail, parse_line,
    };

    #[test]
    fn parse_line_success() {
        assert_eq!(parse_line("0 3 6 9 12 15"), vec![0, 3, 6, 9, 12, 15]);
    }

    #[test]
    fn diff_line_success() {
        assert_eq!(
            diff_line(parse_line("0 3 6 9 12 15").as_slice()),
            vec![3, 3, 3, 3, 3]
        );
    }

    #[test]
    fn diff_line_until_zero_tail_success() {
        assert_eq!(
            diff_line_until_zero_tail(parse_line("0 3 6 9 12 15").as_slice()),
            vec![15, 3, 0]
        );
        assert_eq!(
            diff_line_until_zero_tail(parse_line("1 3 6 10 15 21").as_slice()),
            vec![21, 6, 1, 0]
        );
        assert_eq!(
            diff_line_until_zero_tail(parse_line("10 13 16 21 30 45").as_slice()),
            vec![45, 15, 6, 2, 0]
        );
    }

    #[test]
    fn diff_line_until_zero_head_success() {
        assert_eq!(
            diff_line_until_zero_head(parse_line("10 13 16 21 30 45").as_slice()),
            vec![10, 3, 0, 2, 0]
        );
    }

    #[test]
    fn extrapolate_lines_tail_success() {
        let line_ends1 = diff_line_until_zero_tail(parse_line("0 3 6 9 12 15").as_slice());
        assert_eq!(extrapolate_lines_tail(&line_ends1, 0), 18);
        let line_ends2 = diff_line_until_zero_tail(parse_line("1 3 6 10 15 21").as_slice());
        assert_eq!(extrapolate_lines_tail(&line_ends2, 0), 28);
        let line_ends3 = diff_line_until_zero_tail(parse_line("10 13 16 21 30 45").as_slice());
        assert_eq!(extrapolate_lines_tail(&line_ends3, 0), 68);
    }

    #[test]
    fn extrapolate_lines_head_success() {
        let line_ends1 = diff_line_until_zero_head(parse_line("0 3 6 9 12 15").as_slice());
        assert_eq!(extrapolate_lines_head(&line_ends1, 0), -3);
        let line_ends2 = diff_line_until_zero_head(parse_line("1 3 6 10 15 21").as_slice());
        assert_eq!(extrapolate_lines_head(&line_ends2, 0), 0);
        let line_ends3 = diff_line_until_zero_head(parse_line("10 13 16 21 30 45").as_slice());
        assert_eq!(extrapolate_lines_head(&line_ends3, 0), 5);
    }

    #[test]
    fn do_part1_success() {
        let lines = vec!["0 3 6 9 12 15", "1 3 6 10 15 21", "10 13 16 21 30 45"];
        assert_eq!(do_part1(lines.as_slice()), 114);
    }

    #[test]
    fn do_part2_success() {
        let lines = vec!["0 3 6 9 12 15", "1 3 6 10 15 21", "10 13 16 21 30 45"];
        assert_eq!(do_part2(lines.as_slice()), 2);
    }
}
