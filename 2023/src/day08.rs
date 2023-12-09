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

fn advance_key(
    pos: &String,
    network: &HashMap<String, (String, String)>,
    direction: &Direction,
) -> String {
    let edges = network.get(pos).unwrap();
    String::from(match direction {
        Direction::Left => &edges.0,
        Direction::Right => &edges.1,
    })
}

fn advance_keys(
    pos: &Vec<String>,
    network: &HashMap<String, (String, String)>,
    direction: &Direction,
) -> Vec<String> {
    pos.iter()
        .map(|p| advance_key(p, network, direction))
        .collect()
}

fn gcd(x: i64, y: i64) -> i64 {
    let mut a = x;
    let mut b = y;
    while a != b {
        if a > b {
            a = a - b;
        } else {
            b = b - a;
        }
    }
    a
}

fn lcm(x: i64, y: i64) -> i64 {
    (x * y).abs() / gcd(x, y)
}

pub fn do_part2(lines: &[&str]) -> i64 {
    let map = Map::parse(lines);
    let starting_keys: Vec<String> = map
        .network
        .keys()
        .filter(|k| k.ends_with("A"))
        .map(String::from)
        .collect();
    println!("Starting keys: {:?}", starting_keys);
    starting_keys
        .iter()
        .map(|start_pos| {
            let mut pos = String::from(start_pos);
            let mut idir = 0;
            let mut count = 0;
            while !pos.ends_with("Z") {
                pos = advance_key(&pos, &map.network, &map.instructions[idir]);
                // println!("{:?}", pos);
                idir = (idir + 1) % map.instructions.len();
                count += 1;
            }
            count
        })
        .fold(1, lcm)
}

#[cfg(test)]
mod tests {
    use crate::day08::{do_part2, gcd, lcm, parse_instructions, Direction, Map, Node};
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
    fn do_part2_success() {
        let lines = vec![
            "LR",
            "",
            "11A = (11B, XXX)",
            "11B = (XXX, 11Z)",
            "11Z = (11B, XXX)",
            "22A = (22B, XXX)",
            "22B = (22C, 22C)",
            "22C = (22Z, 22Z)",
            "22Z = (22B, 22B)",
            "XXX = (XXX, XXX)",
        ];
        assert_eq!(do_part2(&lines), 6);
    }

    #[test]
    fn gcd_success() {
        assert_eq!(gcd(48, 18), 6);
        assert_eq!(gcd(48, 47), 1);
    }

    #[test]
    fn lcm_success() {
        assert_eq!(lcm(4, 6), 12);
    }
}
