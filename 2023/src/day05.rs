#[derive(Debug, PartialEq)]
struct AlmanacRange {
    destination_start: i64,
    source_start: i64,
    len: i64,
}

// Parse an usize from s or panic.
fn parse_usize(s: &str) -> usize {
    s.parse::<usize>().unwrap()
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

#[derive(Debug, PartialEq)]
pub struct SeedRange {
    start: i64,
    len: i64,
}

pub fn read_seeds(line: &str) -> Vec<SeedRange> {
    let seed_ranges: Vec<i64> = line
        .split(": ")
        .nth(1)
        .unwrap()
        .split_whitespace()
        .map(parse_i64)
        .collect();
    let mut result: Vec<SeedRange> = Vec::new();
    for i in (0..seed_ranges.len()).into_iter().filter(|x| x % 2 == 0) {
        result.push(SeedRange {
            start: seed_ranges[i] as i64,
            len: seed_ranges[i + 1],
        });
    }
    result
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

pub fn map_seeds(seed_ranges: &[SeedRange], maps: &[AlmanacMap]) -> i64 {
    let mut smallest_result = std::i64::MAX;
    for seed_range in seed_ranges {
        println!("{:?}", seed_range);
        for seed_i in seed_range.start..seed_range.start + seed_range.len {
            let before = seed_i;
            let mut mapped = seed_i;
            for map in maps {
                for almanac_range in map.ranges.iter() {
                    if mapped >= almanac_range.source_start
                        && mapped < almanac_range.source_start + almanac_range.len
                    {
                        let offset = mapped - almanac_range.source_start;
                        mapped = almanac_range.destination_start + offset;
                        break;
                    }
                    // println!("{} -> {}", before, mapped);
                }
            }
            smallest_result = std::cmp::min(mapped, smallest_result);
        }
        println!("smallest_result={}", smallest_result);
    }
    smallest_result
}

#[cfg(test)]
mod tests {
    use crate::day05::{map_seeds, read_maps, read_seeds, AlmanacMap, AlmanacRange, SeedRange};

    #[test]
    fn read_seeds_success() {
        let line = "seeds: 79 14 55 13";
        assert_eq!(
            read_seeds(&line),
            vec![
                SeedRange { start: 79, len: 14 },
                SeedRange { start: 55, len: 13 }
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
    fn map_seeds_success() {
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
        assert_eq!(map_seeds(&seeds, &maps), 46);
    }
}
