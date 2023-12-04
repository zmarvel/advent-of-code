use std::char;

fn is_symbol(c: char) -> bool {
    return !c.is_numeric() && !c.is_alphabetic() && c.is_ascii() && c != '.';
}

pub fn scan_line(prev_line: &str, line: &str, next_line: &str) -> Vec<Vec<char>> {
    let ascii_line: Vec<char> = line.chars().collect();
    if prev_line.len() != ascii_line.len() || next_line.len() != ascii_line.len() {
        panic!("Invalid len");
    }

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
        .iter()
        .filter(|(num_start, num_end)| {
            let min_neighbor = if *num_start == 0usize {
                0
            } else {
                num_start - 1
            };
            let max_neighbor = std::cmp::min(ascii_line.len(), num_end + 1);
            prev_line[(min_neighbor..max_neighbor)]
                .chars()
                .any(is_symbol)
                || next_line[(min_neighbor..max_neighbor)]
                    .chars()
                    .any(is_symbol)
                || line[(min_neighbor..max_neighbor)].chars().any(is_symbol)
        })
        .map(|(num_start, num_end)| {
            let slice = &ascii_line[*num_start..*num_end];
            Vec::from(slice)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::day03::scan_line;

    #[test]
    fn scan_line_skip() {
        let prev_line = ".......";
        let curr_line = "12...34";
        let next_line = ".......";
        assert_eq!(
            scan_line(&prev_line, &curr_line, &next_line),
            Vec::<Vec<char>>::new()
        );
    }

    #[test]
    fn scan_line_include() {
        let prev_line = "..!........";
        let curr_line = "12...34.56@";
        let next_line = "....#......";
        assert_eq!(
            scan_line(&prev_line, &curr_line, &next_line),
            vec!(
                String::from("12").chars().collect::<Vec<char>>(),
                String::from("34").chars().collect::<Vec<char>>(),
                String::from("56").chars().collect::<Vec<char>>(),
            )
        );
    }
}
