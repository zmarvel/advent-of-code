use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
enum Direction {
    Left,
    Right,
}

fn parse_instructions(line: &str) -> Vec<Direction> {
    line.as_bytes()
        .iter()
        .map(|&b| b as char)
        .map(|c| match c {
            'L' => Direction::Left,
            'R' => Direction::Right,
            _ => panic!("Invalid direction"),
        })
        .collect()
}

fn parse_ends(ends: &str) -> (String, String) {
    let mut ends_iter = ends.split(", ");
    (
        String::from(ends_iter.next().unwrap().strip_prefix("(").unwrap()),
        String::from(ends_iter.next().unwrap().strip_suffix(")").unwrap()),
    )
}

#[derive(Debug, PartialEq, Eq)]
struct Node {
    start: String,
    ends: (String, String),
}

impl Node {
    fn parse(line: &str) -> Node {
        let mut edge_iter = line.split(" = ");
        let start = edge_iter.next().unwrap();
        Node {
            start: String::from(start),
            ends: parse_ends(edge_iter.next().unwrap()),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Map {
    instructions: Vec<Direction>,
    network: HashMap<String, (String, String)>,
}

impl Map {
    fn parse(lines: &[&str]) -> Map {
        let instructions = parse_instructions(lines[0]);
        let mut network = HashMap::new();
        for line in lines[2..].iter() {
            let node = Node::parse(line);
            network.insert(node.start, node.ends);
        }
        Map {
            instructions,
            network,
        }
    }
}

pub fn do_part1(lines: &[&str]) -> i64 {
    let map = Map::parse(lines);
    let mut pos = "AAA";
    let mut idir = 0;
    let mut count = 0;
    while pos != "ZZZ" {
        pos = match map.instructions[idir] {
            Direction::Left => map.network.get(pos).unwrap().0.as_str(),
            Direction::Right => map.network.get(pos).unwrap().1.as_str(),
        };

        idir = (idir + 1) % map.instructions.len();
        count += 1;
    }

    count
}

#[cfg(test)]
mod tests {
    use crate::day08::{do_part1, parse_instructions, Direction, Map, Node};
    use std::collections::HashMap;

    #[test]
    fn parse_instructions_success() {
        assert_eq!(
            parse_instructions("LLR"),
            vec![Direction::Left, Direction::Left, Direction::Right]
        );
    }

    #[test]
    fn parse_node_success() {
        assert_eq!(
            Node::parse("AAA = (BBB, CCC)"),
            Node {
                start: String::from("AAA"),
                ends: (String::from("BBB"), String::from("CCC")),
            }
        );
    }

    #[test]
    fn parse_map_success() {
        let lines = vec![
            "RL",
            "",
            "AAA = (BBB, CCC)",
            "BBB = (DDD, EEE)",
            "CCC = (ZZZ, GGG)",
            "DDD = (DDD, DDD)",
            "EEE = (EEE, EEE)",
            "GGG = (GGG, GGG)",
            "ZZZ = (ZZZ, ZZZ)",
        ];
        assert_eq!(
            Map::parse(&lines),
            Map {
                instructions: vec![Direction::Right, Direction::Left],
                network: {
                    let mut network = HashMap::new();
                    network.insert(
                        String::from("AAA"),
                        (String::from("BBB"), String::from("CCC")),
                    );
                    network.insert(
                        String::from("BBB"),
                        (String::from("DDD"), String::from("EEE")),
                    );
                    network.insert(
                        String::from("CCC"),
                        (String::from("ZZZ"), String::from("GGG")),
                    );
                    network.insert(
                        String::from("DDD"),
                        (String::from("DDD"), String::from("DDD")),
                    );
                    network.insert(
                        String::from("EEE"),
                        (String::from("EEE"), String::from("EEE")),
                    );
                    network.insert(
                        String::from("GGG"),
                        (String::from("GGG"), String::from("GGG")),
                    );
                    network.insert(
                        String::from("ZZZ"),
                        (String::from("ZZZ"), String::from("ZZZ")),
                    );
                    network
                }
            }
        );
    }

    #[test]
    fn do_part1_success() {
        let lines1 = vec![
            "RL",
            "",
            "AAA = (BBB, CCC)",
            "BBB = (DDD, EEE)",
            "CCC = (ZZZ, GGG)",
            "DDD = (DDD, DDD)",
            "EEE = (EEE, EEE)",
            "GGG = (GGG, GGG)",
            "ZZZ = (ZZZ, ZZZ)",
        ];
        assert_eq!(do_part1(&lines1), 2);

        let lines2 = vec![
            "LLR",
            "",
            "AAA = (BBB, BBB)",
            "BBB = (AAA, ZZZ)",
            "ZZZ = (ZZZ, ZZZ)",
        ];
        assert_eq!(do_part1(&lines2), 6);
    }
}
