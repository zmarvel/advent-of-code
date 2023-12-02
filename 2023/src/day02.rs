#[derive(Default, Debug, PartialEq)]
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

// Returns the line ID if
pub fn parse_line(requirements: &DiceCount, line: &str) -> Option<i64> {
    // Game 100: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    let id_and_rounds: Vec<&str> = line.split(": ").collect();
    // Game 100
    let id = id_and_rounds
        .get(0)
        .unwrap()
        .split_whitespace()
        .collect::<Vec<&str>>()
        .get(1)
        .unwrap()
        .parse::<i64>()
        .unwrap();

    // 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    for round in id_and_rounds.get(1).unwrap().split("; ") {
        let count = parse_round(&round);
        if count.red > requirements.red
            || count.blue > requirements.blue
            || count.green > requirements.green
        {
            return None;
        }
    }
    Some(id)
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
    fn parse_line_meets_requirements() {
        let requirements = DiceCount {
            red: 4,
            green: 2,
            blue: 6,
        };
        let line = "Game 12: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        assert_eq!(parse_line(&requirements, &line), Some(12));
    }

    #[test]
    fn parse_line_doesnt_meet_requirements() {
        let requirements = DiceCount {
            red: 3,
            green: 2,
            blue: 6,
        };
        let line = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        assert_eq!(parse_line(&requirements, &line), None);
    }
}
