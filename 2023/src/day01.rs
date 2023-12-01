use std::string::String;

pub fn extract_calibration_value(line: &String) -> i64 {
    let first = line.find(char::is_numeric).expect("first calibration value");
    let second = line.rfind(char::is_numeric).expect("second calibration value");

    let first_num = line.get(first..(first+1)).unwrap().parse::<i64>().unwrap();
    let second_num = line.get(second..(second+1)).unwrap().parse::<i64>().unwrap();

    return first_num * 10 + second_num
}

// TODO: Refactor so this takes some sort of iterator? Or call the above helper in a loop.
pub fn sum_calibration_values(lines: &[String]) -> i64 {
    lines.iter().map(extract_calibration_value).fold(0, |acc, x| acc + x)
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
    fn sum_calibration_values_full() {
        let inputs = vec![String::from("1abc2"), String::from("pqr3stu8vwx"),String::from("a1b2c3d4e5f"),String::from("treb7uchet")];
        assert_eq!(sum_calibration_values(&inputs), 142);
    }
}
