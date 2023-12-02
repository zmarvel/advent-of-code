use std::string::String;

const NUMBER_WORDS: [&str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

const NUMBER_NUMBERS: [i64; 9] = [1, 2, 3, 4, 5, 6, 7, 8, 9];

fn find_number_words(line: &str) -> (Option<usize>, Option<usize>) {
    let mut least_word_pos = None;
    let mut least_word_idx = None;
    for (i, word) in NUMBER_WORDS.iter().enumerate() {
        if let Some(word_pos) = line.find(word) {
            if word_pos < least_word_pos.unwrap_or(line.len() - 1) {
                least_word_pos = Some(word_pos);
                least_word_idx = Some(i);
            }
        }
    }
    (least_word_idx, least_word_pos)
}

fn rfind_number_words(line: &str) -> (Option<usize>, Option<usize>) {
    let mut greatest_word_pos = None;
    let mut greatest_word_idx = None;
    for (i, word) in NUMBER_WORDS.iter().enumerate() {
        if let Some(word_pos) = line.rfind(word) {
            if word_pos > greatest_word_pos.unwrap_or(0) {
                greatest_word_pos = Some(word_pos);
                greatest_word_idx = Some(i)
            }
        }
    }
    (greatest_word_idx, greatest_word_pos)
}

fn get_pos_or_max(maybe_pos: Option<usize>, line: &String) -> usize {
    maybe_pos.unwrap_or(line.len() - 1)
}

fn get_pos_or_min(maybe_pos: Option<usize>) -> usize {
    maybe_pos.unwrap_or(0)
}

pub fn extract_calibration_value(line: &String) -> i64 {
    let first_number_pos = line.find(char::is_numeric);
    let (first_word_idx, first_word_pos) = find_number_words(line);
    let first_num = if get_pos_or_max(first_word_pos, line) < get_pos_or_max(first_number_pos, line)
    {
        *NUMBER_NUMBERS.get(first_word_idx.unwrap()).unwrap()
    } else {
        let first_pos = first_number_pos.unwrap();
        line.get(first_pos..(first_pos + 1))
            .unwrap()
            .parse::<i64>()
            .unwrap()
    };

    let second_number_pos = line.rfind(char::is_numeric);
    let (second_word_idx, second_word_pos) = rfind_number_words(line);
    let second_num = if get_pos_or_min(second_word_pos) > get_pos_or_min(second_number_pos) {
        *NUMBER_NUMBERS.get(second_word_idx.unwrap()).unwrap()
    } else {
        let second_pos = second_number_pos.unwrap();
        line.get(second_pos..(second_pos + 1))
            .unwrap()
            .parse::<i64>()
            .unwrap()
    };

    return first_num * 10 + second_num;
}

fn sum_calibration_values(lines: &[String]) -> i64 {
    lines
        .iter()
        .map(extract_calibration_value)
        .fold(0, |acc, x| acc + x)
}

#[cfg(test)]
mod tests {
    use crate::day01::sum_calibration_values;

    #[test]
    fn sum_calibration_values_basic() {
        let inputs = vec![String::from("1abc2")];
        assert_eq!(sum_calibration_values(&inputs), 12);
    }

    #[test]
    fn sum_calibration_values_numeric_words() {
        let inputs = vec![
            String::from("two1nine"),
            String::from("eightwothree"),
            String::from("abcone2threexyz"),
            String::from("xtwone3four"),
            String::from("4nineeightseven2"),
            String::from("zoneight234"),
            String::from("7pqrstsixteen"),
        ];
        assert_eq!(sum_calibration_values(&inputs), 281);
    }

    #[test]
    fn sum_calibration_values_full() {
        let inputs = vec![
            String::from("1abc2"),
            String::from("pqr3stu8vwx"),
            String::from("a1b2c3d4e5f"),
            String::from("treb7uchet"),
        ];
        assert_eq!(sum_calibration_values(&inputs), 142);
    }
}
