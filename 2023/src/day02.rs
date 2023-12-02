#[derive(Default, Debug, PartialEq, Copy, Clone)]
pub struct DiceCount {
    pub red: i64,
    pub green: i64,
    pub blue: i64,
}

fn parse_round(round: &str) -> DiceCount {
    let mut count = DiceCount::default();
    for roll in round.split(", ") {
        let mut num_and_color = roll.split_whitespace();
        let num = num_and_color.next().unwrap().parse::<i64>().unwrap();
        let color = num_and_color.next().unwrap();
        if color == "red" {
            count.red += num;
        } else if color == "green" {
            count.green += num;
        } else if color == "blue" {
            count.blue += num;
        }
    }
    count
}

fn update_min_set(min_set: &DiceCount, round: &DiceCount) -> DiceCount {
    DiceCount {
        red: std::cmp::max(min_set.red, round.red),
        green: std::cmp::max(min_set.green, round.green),
        blue: std::cmp::max(min_set.blue, round.blue),
    }
}

fn min_set_power(count: &DiceCount) -> i64 {
    count.red * count.green * count.blue
}

// Returns the "power" of the line's "minimum set."
pub fn parse_line(line: &str) -> i64 {
    // Game 100: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    let id_and_rounds: Vec<&str> = line.split(": ").collect();

    // 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    let mut min_set = DiceCount::default();
    for round in id_and_rounds.get(1).unwrap().split("; ") {
        let count = parse_round(&round);
        min_set = update_min_set(&min_set, &count);
    }
    min_set_power(&min_set)
}

#[cfg(test)]
mod tests {
    use crate::day02::{parse_line, parse_round, DiceCount};

    #[test]
    fn parse_round_red() {
        let round = "4 red";
        assert_eq!(
            parse_round(&round),
            DiceCount {
                red: 4,
                green: 0,
                blue: 0,
            }
        );
    }

    #[test]
    fn parse_round_green() {
        let round = "4 green";
        assert_eq!(
            parse_round(&round),
            DiceCount {
                red: 0,
                green: 4,
                blue: 0,
            }
        );
    }

    #[test]
    fn parse_round_blue() {
        let round = "4 blue";
        assert_eq!(
            parse_round(&round),
            DiceCount {
                red: 0,
                green: 0,
                blue: 4,
            }
        );
    }

    #[test]
    fn parse_line_min_set() {
        let line = "Game 12: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        assert_eq!(parse_line(&line), 48);
    }
}
