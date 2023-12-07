#[derive(Default, PartialEq, Debug)]
struct RaceRecords {
    time: Vec<i64>,
    distance: Vec<i64>,
}

fn parse_line(line: &str) -> Vec<i64> {
    line.split(":")
        .nth(1)
        .unwrap()
        .split_whitespace()
        .map(|n| n.parse::<i64>().unwrap())
        .collect()
}

fn parse_lines(lines: &[&str]) -> RaceRecords {
    let time_line = &lines[0];
    let distance_line = &lines[1];
    RaceRecords {
        time: parse_line(time_line),
        distance: parse_line(distance_line),
    }
}

struct RaceRecords2 {
    time: i64,
    distance: i64,
}

fn parse_line2(line: &str) -> i64 {
    line.split(":")
        .nth(1)
        .unwrap()
        .split_whitespace()
        .collect::<Vec<&str>>()
        .join("")
        .parse::<i64>()
        .unwrap()
}

fn parse_lines2(lines: &[&str]) -> RaceRecords2 {
    let time_line = &lines[0];
    let distance_line = &lines[1];
    RaceRecords2 {
        time: parse_line2(time_line),
        distance: parse_line2(distance_line),
    }
}

// x * (t - x) = x*t - x^2
fn calculate_final_distance(time: i64, button_time: i64) -> i64 {
    button_time * (time - button_time)
}

fn is_winning(time: i64, button_time: i64, record_distance: i64) -> bool {
    calculate_final_distance(time, button_time) > record_distance
}

// -x^2 + t*x = d

fn count_winning_strategies(time: i64, record_distance: i64) -> i64 {
    let mut start = 0;
    while !is_winning(time, start, record_distance) {
        start += 1;
    }

    let mut i = start;
    while is_winning(time, i, record_distance) {
        i += 1;
    }

    i - start
}

pub fn do_part1(line_strings: &[String]) -> i64 {
    let lines: Vec<&str> = line_strings.iter().map(|s| s.as_str()).collect();
    let records = parse_lines(&lines);
    let mut result = 1;
    for i in 0..records.time.len() {
        let winning = count_winning_strategies(records.time[i], records.distance[i]);
        println!(
            "time: {} distance: {} winning: {}",
            records.time[i], records.distance[i], winning
        );
        result *= winning;
    }
    result
}

pub fn do_part2(line_strings: &[String]) -> i64 {
    let lines: Vec<&str> = line_strings.iter().map(|s| s.as_str()).collect();
    let records = parse_lines2(&lines);
    count_winning_strategies(records.time, records.distance)
}

#[cfg(test)]
mod tests {
    use crate::day06::{
        calculate_final_distance, count_winning_strategies, do_part1, do_part2, is_winning,
        parse_line, parse_lines, RaceRecords,
    };

    #[test]
    fn parse_line_success() {
        let line = "Time:      7  15   30";
        assert_eq!(parse_line(&line), vec![7, 15, 30]);
    }

    #[test]
    fn parse_lines_success() {
        let lines = vec!["Time:      7  15   30", "Distance:  9  40  200"];
        assert_eq!(
            parse_lines(&lines),
            RaceRecords {
                time: vec![7, 15, 30],
                distance: vec![9, 40, 200],
            }
        );
    }

    #[test]
    fn calculate_final_distance_success() {
        assert_eq!(calculate_final_distance(7, 1), 6);
        assert_eq!(calculate_final_distance(7, 2), 10);
        assert_eq!(calculate_final_distance(7, 3), 12);
        assert_eq!(calculate_final_distance(7, 4), 12);
        assert_eq!(calculate_final_distance(7, 5), 10);
        assert_eq!(calculate_final_distance(7, 6), 6);
    }

    #[test]
    fn is_winning_success() {
        assert_eq!(is_winning(7, 1, 9), false);
        assert_eq!(is_winning(7, 2, 9), true);
        assert_eq!(is_winning(7, 3, 9), true);
        assert_eq!(is_winning(7, 4, 9), true);
        assert_eq!(is_winning(7, 5, 9), true);
        assert_eq!(is_winning(7, 6, 9), false);
    }

    #[test]
    fn count_winning_strategies_success() {
        assert_eq!(count_winning_strategies(7, 9), 4);
        assert_eq!(count_winning_strategies(15, 40), 8);
        // -x^2 + t*x = d
        // for t=30 and d=200
        // 7 -> -7*7 + 30*7 = 210 - 49
        // 8 -> -8*8 + 30*8 = 240 - 64
        // 9 -> -9*9 + 30*9 = 270 - 81
        // Ah, so in this case, when we solve for x given d=200, we get ~8.3.
        // This doesn't lead us to a winning search space, so we need a
        // different strategy to find a starting parameter.
        assert_eq!(count_winning_strategies(30, 200), 9);
    }

    #[test]
    fn do_part1_success() {
        let lines: Vec<String> = vec!["Time:      7  15   30", "Distance:  9  40  200"]
            .iter()
            .map(|&s| String::from(s))
            .collect();
        assert_eq!(do_part1(&lines), 288);
    }

    #[test]
    fn do_part2_success() {
        let lines: Vec<String> = vec!["Time:      7  15   30", "Distance:  9  40  200"]
            .iter()
            .map(|&s| String::from(s))
            .collect();
        assert_eq!(do_part2(&lines), 71503);
    }
}
