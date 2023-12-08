#[derive(Debug, Default, PartialEq, Eq)]
struct Hand {
    cards: Vec<char>,
    bid: i64,
}

fn get_card_num(card: char) -> usize {
    match card {
        'A' => 12,
        'K' => 11,
        'Q' => 10,
        // 'J' => 9,
        'T' => 9,
        '2'..='9' => (card as usize) - ('2' as usize) + 1,
        'J' => 0,
        _ => panic!("Invalid card: {:?}", card),
    }
}

fn upgrade_hand_type(card_count: &[i64], hand_type: HandType) -> HandType {
    let num_jokers: i64 = card_count[get_card_num('J')];
    use HandType::*;
    match num_jokers {
        0 => hand_type,
        1 => match hand_type {
            HighCard => OnePair,
            OnePair => ThreeOfAKind,
            TwoPair => FullHouse,
            ThreeOfAKind => FourOfAKind,
            FourOfAKind => FiveOfAKind,
            _ => panic!(),
        },
        2 => match hand_type {
            OnePair => ThreeOfAKind,
            TwoPair => FourOfAKind,
            FullHouse => FiveOfAKind,
            _ => panic!(),
        },
        3 => match hand_type {
            ThreeOfAKind => FourOfAKind,
            FullHouse => FiveOfAKind,
            _ => panic!(),
        },
        4 => match hand_type {
            FourOfAKind => FiveOfAKind,
            _ => panic!(),
        },
        5 => hand_type,
        _ => panic!(),
    }
}

#[derive(Debug, PartialEq, Eq)]
enum HandType {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
}

impl HandType {
    fn get_number(&self) -> i64 {
        match self {
            Self::FiveOfAKind => 7,
            Self::FourOfAKind => 6,
            Self::FullHouse => 5,
            Self::ThreeOfAKind => 4,
            Self::TwoPair => 3,
            Self::OnePair => 2,
            Self::HighCard => 1,
        }
    }
}

impl Hand {
    fn parse(line: &str) -> Hand {
        let mut line_iter = line.split_whitespace();
        Hand {
            cards: line_iter
                .next()
                .unwrap()
                .as_bytes()
                .iter()
                .map(|&b| b as char)
                .collect(),
            bid: line_iter.next().unwrap().parse::<i64>().unwrap(),
        }
    }

    fn hand_type(&self) -> HandType {
        let card_count = self.count_cards();
        if let Some(hand_type) = Hand::hand_type_from_card_count(card_count.as_slice()) {
            upgrade_hand_type(card_count.as_slice(), hand_type)
        } else {
            panic!("Invalid type for hand {:?}", self.cards);
        }
    }

    fn hand_type_from_card_count(card_count: &[i64]) -> Option<HandType> {
        use HandType::*;
        if card_count.iter().any(|&count| count == 5) {
            Some(FiveOfAKind)
        } else if card_count.iter().any(|&count| count == 4)
            && card_count.iter().any(|&count| count == 1)
        {
            Some(FourOfAKind)
        } else if card_count.iter().any(|&count| count == 3)
            && card_count.iter().any(|&count| count == 2)
        {
            Some(FullHouse)
        } else if card_count.iter().any(|&count| count == 3)
            && card_count
                .iter()
                .filter(|&&count| count < 2 && count > 0)
                .count()
                == 2
        {
            Some(ThreeOfAKind)
        } else if card_count.iter().filter(|&&count| count == 2).count() == 2 {
            Some(TwoPair)
        } else if card_count.iter().filter(|&&count| count == 2).count() == 1
            && card_count
                .iter()
                .filter(|&&count| count < 2 && count > 0)
                .count()
                == 3
        {
            Some(OnePair)
        } else if card_count.iter().all(|&count| count < 2) {
            Some(HighCard)
        } else {
            None
        }
    }

    fn count_cards(&self) -> Vec<i64> {
        let mut count = Vec::new();
        count.resize(get_card_num('A') + 1, 0);
        for &card in self.cards.as_slice() {
            count[get_card_num(card)] += 1;
        }
        count
    }

    fn compare_cards(&self, other: &Self) -> std::cmp::Ordering {
        let our_cards = self.cards.iter().map(|&c| get_card_num(c));
        let other_cards = other.cards.iter().map(|&c| get_card_num(c));
        our_cards.cmp(other_cards)
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let our_type = self.hand_type();
        let other_type = other.hand_type();
        let type_cmp = our_type.get_number().cmp(&other_type.get_number());
        if type_cmp.is_eq() {
            self.compare_cards(other)
        } else {
            type_cmp
        }
    }
}

fn parse_hands(lines: &[&str]) -> Vec<Hand> {
    lines.iter().map(|s| Hand::parse(s)).collect()
}

pub fn do_part1(lines: &[&str]) -> i64 {
    let mut indices = (0..lines.len()).collect::<Vec<usize>>();
    let hands = parse_hands(lines);
    indices.sort_by_key(|&i| &hands[i]);
    indices
        .iter()
        .enumerate()
        .map(|(i, &ihand)| ((i as i64) + 1) * hands[ihand].bid)
        .fold(0, |acc, x| acc + x)
}

#[cfg(test)]
mod tests {
    use crate::day07::{do_part1, parse_hands, Hand, HandType};

    // 32T3K 765
    // T55J5 684
    // KK677 28
    // KTJJT 220
    // QQQJA 483

    #[test]
    fn parse_hand_success() {
        let line = "32T3K 765";
        assert_eq!(
            Hand::parse(&line),
            Hand {
                cards: vec!['3', '2', 'T', '3', 'K'],
                bid: 765
            }
        );
    }

    #[test]
    fn parse_hands_success() {
        let lines = vec!["32T3K 765", "T55J5 684"];
        assert_eq!(
            parse_hands(lines.as_slice()),
            vec![
                Hand {
                    cards: vec!['3', '2', 'T', '3', 'K'],
                    bid: 765
                },
                Hand {
                    cards: vec!['T', '5', '5', 'J', '5'],
                    bid: 684
                }
            ]
        );
    }

    #[test]
    fn hand_type_success() {
        let lines = vec![
            "32T3K 765",
            "T55J5 684",
            "KK677 28",
            "KTJJT 220",
            "QQQJA 483",
        ];
        let hands = parse_hands(lines.as_slice());
        use HandType::*;
        assert_eq!(
            hands
                .iter()
                .map(|hand| hand.hand_type())
                .collect::<Vec<HandType>>(),
            vec![OnePair, FourOfAKind, TwoPair, FourOfAKind, FourOfAKind]
        );
    }

    #[test]
    fn cmp_hands_success() {
        let lines = vec![
            "32T3K 765", // 0
            "T55J5 684", // 1
            "KK677 28",  // 2
            "KTJJT 220", // 3
            "QQQJA 483", // 4
        ];
        let mut indices = (0..lines.len()).collect::<Vec<usize>>();
        let hands = parse_hands(lines.as_slice());
        indices.sort_by_key(|&i| &hands[i]);
        // 32T3K: 0
        // KK677: 2
        // T55J5: 1
        // QQQJA: 4
        // KTJJT: 3
        assert_eq!(indices, vec![0, 2, 1, 4, 3]);
    }

    #[test]
    fn do_part1_success() {
        let lines = vec![
            "32T3K 765",
            "T55J5 684",
            "KK677 28",
            "KTJJT 220",
            "QQQJA 483",
        ];
        assert_eq!(do_part1(lines.as_slice()), 5905);
    }
}
