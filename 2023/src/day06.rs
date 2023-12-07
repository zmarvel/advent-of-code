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

// x * (t - x) = x*t - x^2
fn calculate_final_distance(time: i64, button_time: i64) -> i64 {
    button_time * (time - button_time)
}

fn is_winning(time: i64, button_time: i64, record_distance: i64) -> bool {
    calculate_final_distance(time, button_time) > record_distance
}

// -x^2 + t*x = d
// -x^2 + t*x - d = 0

// -x^2 + t*x > d
// x * (-x + t) > d
// -x + t > d / x

// -x^2 + 7*x > 9
// x * (-x + 7) > 9
// -x + 7 > 9 / x
// -x > 9 / x - 7
// x < -9 / x + 7
// -x^2 > 9 - 7*x

// -x^2 + 30*x > 200

fn solve_winning_button_time(time: i64, record_distance: i64) -> (i64, i64) {
    let pos_solution = ((-time + (time * time - 4 * -1 * -record_distance)) as f64).sqrt();
    let neg_solution = ((-time - (time * time - 4 * -1 * -record_distance)) as f64).sqrt();
    println!(
        "pos_solution={} neg_solution={}",
        pos_solution, neg_solution
    );
    (pos_solution.round() as i64, neg_solution.round() as i64)
}

fn check_solution(time: i64, record_distance: i64, winning_time: i64) -> i64 {
    if winning_time < 0 {
        return 0;
    }

    let start_rev = winning_time - 1;
    let mut i_rev = start_rev;
    while i_rev > 0 && is_winning(time, i_rev, record_distance) {
        i_rev -= 1;
    }
    let rev_count = start_rev - i_rev;

    let start_fwd = winning_time + 1;
    let mut i_fwd = start_fwd;
    while is_winning(time, i_fwd, record_distance) {
        i_fwd += 1;
    }
    let fwd_count = i_fwd - start_fwd;
    println!("rev_count={}, fwd_count={}", rev_count, fwd_count);

    fwd_count
        + rev_count
        + if is_winning(time, winning_time, record_distance) {
            1
        } else {
            0
        }
}

fn count_winning_strategies(time: i64, record_distance: i64) -> i64 {
    let (pos_solution, neg_solution) = solve_winning_button_time(time, record_distance);
    check_solution(time, record_distance, pos_solution)
        + check_solution(time, record_distance, neg_solution)
}

#[cfg(test)]
mod tests {
    use crate::day06::{
        calculate_final_distance, count_winning_strategies, is_winning, parse_line, parse_lines,
        RaceRecords,
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
}
