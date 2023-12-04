use std::collections::HashSet;

struct Scratchcard {
    have_numbers: HashSet<i64>,
    winning_numbers: HashSet<i64>,
}

impl Scratchcard {
    fn from_line(line: &str) -> Scratchcard {
        let mut parts = line.split(": ").nth(1).unwrap().split(" | ");
        let winning_numbers = parts
            .next()
            .unwrap()
            .split_whitespace()
            .map(|s| s.parse::<i64>().unwrap())
            .collect();
        let have_numbers = parts
            .next()
            .unwrap()
            .split_whitespace()
            .map(|s| s.parse::<i64>().unwrap())
            .collect();
        Scratchcard {
            have_numbers,
            winning_numbers,
        }
    }

    fn count_winning_numbers(&self) -> i64 {
        self.winning_numbers
            .intersection(&self.have_numbers)
            .count() as i64
    }
}

fn count_winning_numbers(lines: &Vec<&str>) -> Vec<i64> {
    lines
        .iter()
        .map(|line| Scratchcard::from_line(line))
        .map(|card| card.count_winning_numbers())
        .collect()
}

pub fn calculate_score(lines: &Vec<&str>) -> i64 {
    count_winning_numbers(lines)
        .iter()
        .filter(|&&n| n > 0)
        .map(|num_winning| 2i64.pow(*num_winning as u32 - 1))
        .fold(0, |acc, x| acc + x)
}

#[cfg(test)]
mod tests {
    use crate::day04::{calculate_score, count_winning_numbers, Scratchcard};
    use std::collections::HashSet;

    #[test]
    fn scratchcard_from_line() {
        let line = "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1";
        let result = Scratchcard::from_line(&line);
        let mut expected_have_numbers = HashSet::new();
        expected_have_numbers.insert(69);
        expected_have_numbers.insert(82);
        expected_have_numbers.insert(82);
        expected_have_numbers.insert(63);
        expected_have_numbers.insert(72);
        expected_have_numbers.insert(16);
        expected_have_numbers.insert(21);
        expected_have_numbers.insert(14);
        expected_have_numbers.insert(1);
        let mut expected_winning_numbers = HashSet::new();
        expected_winning_numbers.insert(1);
        expected_winning_numbers.insert(21);
        expected_winning_numbers.insert(53);
        expected_winning_numbers.insert(59);
        expected_winning_numbers.insert(44);

        assert_eq!(result.have_numbers, expected_have_numbers);
        assert_eq!(result.winning_numbers, expected_winning_numbers);
    }

    #[test]
    fn scratchcard_count_winning_numbers() {
        let line = "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1";
        assert_eq!(Scratchcard::from_line(&line).count_winning_numbers(), 2);
    }

    #[test]
    fn count_winning_numbers_test() {
        let lines = vec![
            "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
            "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
            "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
            "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
            "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
            "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11",
        ];
        assert_eq!(count_winning_numbers(&lines), vec![4, 2, 2, 1, 0, 0]);
    }

    #[test]
    fn calculate_score_test() {
        let lines = vec![
            "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
            "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
            "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
            "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
            "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
            "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11",
        ];
        assert_eq!(calculate_score(&lines), 13);
    }
}
