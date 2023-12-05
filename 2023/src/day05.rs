#[derive(Debug, PartialEq)]
struct AlmanacRange {
    destination_start: i64,
    source_start: i64,
    len: i64,
}

// Parse an i64 from s or panic.
fn parse_i64(s: &str) -> i64 {
    s.parse::<i64>().unwrap()
}

impl AlmanacRange {
    fn from_line(line: &str) -> AlmanacRange {
        let mut line_iter = line.split_whitespace();
        AlmanacRange {
            destination_start: parse_i64(line_iter.next().unwrap()),
            source_start: parse_i64(line_iter.next().unwrap()),
            len: parse_i64(line_iter.next().unwrap()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct AlmanacMap {
    ranges: Vec<AlmanacRange>,
}

impl AlmanacMap {
    fn from_lines(lines: &[&str]) -> AlmanacMap {
        // seed-to-soil map:
        // 50 98 2
        // 52 50 48
        AlmanacMap {
            ranges: lines[1..]
                .iter()
                .map(|line| AlmanacRange::from_line(line))
                .collect(),
        }
    }
}

pub fn read_seeds(line: &str) -> Vec<i64> {
    let seed_ranges: Vec<i64> = line
        .split(": ")
        .nth(1)
        .unwrap()
        .split_whitespace()
        .map(|s| s.parse::<i64>().unwrap())
        .collect();
    let mut seeds: Vec<i64> = Vec::new();
    for i in (0..seed_ranges.len()).into_iter().filter(|x| x % 2 == 0) {
        let start = seed_ranges[i];
        let len = seed_ranges[i + 1];
        for i in 0..len {
            seeds.push(start + i);
        }
    }
    seeds
}

pub fn read_maps(lines: &[String]) -> Vec<AlmanacMap> {
    let seeds = &lines[0];
    // The maps start on the third line.
    let mut maps_lines: Vec<Vec<&str>> = Vec::new();
    let mut map_lines: Vec<&str> = Vec::new();
    for i in 2..lines.len() {
        if lines[i] == "" {
            maps_lines.push(map_lines);
            map_lines = Vec::new();
        } else {
            map_lines.push(lines[i].as_str());
        }
    }
    if map_lines.len() > 0 {
        maps_lines.push(map_lines);
    }

    maps_lines
        .iter()
        .map(|lines| AlmanacMap::from_lines(lines))
        .collect()
}

pub fn map_seeds(seeds: &[i64], maps: &[AlmanacMap]) -> Vec<i64> {
    let mut mapped = Vec::from(seeds);

    for map in maps {
        for i in 0..mapped.len() {
            for range in map.ranges.iter() {
                let before = mapped[i];
                if mapped[i] >= range.source_start && mapped[i] < range.source_start + range.len {
                    let offset = mapped[i] - range.source_start;
                    mapped[i] = range.destination_start + offset;
                    break;
                }
                // println!("{} -> {}", before, mapped[i]);
                // Otherwise mapped[i] -> mapped[i]
            }
        }
    }

    mapped
}

#[cfg(test)]
mod tests {
    use crate::day05::{map_seeds, read_maps, read_seeds, AlmanacMap, AlmanacRange};

    #[test]
    fn read_seeds_success() {
        let line = "seeds: 79 14 55 13";
        assert_eq!(
            read_seeds(&line),
            vec![
                79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 55, 56, 57, 58, 59, 60, 61,
                62, 63, 64, 65, 66, 67
            ]
        );
    }

    #[test]
    fn read_maps_simple() {
        let lines: Vec<String> = vec![
            "seeds: 79 14 55 13",
            "",
            "seed-to-soil map:",
            "50 98 2",
            "52 50 48",
        ]
        .iter()
        .map(|&s| String::from(s))
        .collect();
        assert_eq!(
            read_maps(&lines.as_slice()),
            vec![AlmanacMap {
                ranges: vec![
                    AlmanacRange {
                        destination_start: 50,
                        source_start: 98,
                        len: 2
                    },
                    AlmanacRange {
                        destination_start: 52,
                        source_start: 50,
                        len: 48
                    }
                ]
            }]
        );
    }

    #[test]
    fn read_maps_complex() {
        let lines: Vec<String> = vec![
            "seeds: 79 14 55 13",
            "",
            "seed-to-soil map:",
            "50 98 2",
            "52 50 48",
            "",
            "soil-to-fertilizer map:",
            "0 15 37",
            "37 52 2",
            "39 0 15",
            "",
            "fertilizer-to-water map:",
            "49 53 8",
            "0 11 42",
            "42 0 7",
            "57 7 4",
            "",
            "water-to-light map:",
            "88 18 7",
            "18 25 70",
            "",
            "light-to-temperature map:",
            "45 77 23",
            "81 45 19",
            "68 64 13",
            "",
            "temperature-to-humidity map:",
            "0 69 1",
            "1 0 69",
            "",
            "humidity-to-location map:",
            "60 56 37",
            "56 93 4",
        ]
        .iter()
        .map(|&s| String::from(s))
        .collect();
        let seeds = read_seeds(&lines[0]);
        let maps = read_maps(&lines);
        assert_eq!(*map_seeds(&seeds, &maps).iter().min().unwrap(), 46);
    }
}
