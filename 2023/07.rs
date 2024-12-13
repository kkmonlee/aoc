use std::fs::read_to_string;
use std::collections::HashMap;

#[derive(Clone)]
struct Hand {
    cards: String,
    bid: i64,
}

fn get_card_value(c: char, joker_rules: bool) -> i32 {
    match c {
        'A' => 14,
        'K' => 13,
        'Q' => 12,
        'J' if joker_rules => 1,  // J is lowest in part 2
        'J' => 11,
        'T' => 10,
        _ => c.to_digit(10).unwrap() as i32
    }
}

fn get_hand_type(cards: &str, joker_rules: bool) -> (i32, Vec<i32>) {
    let mut counts = HashMap::new();
    let card_values: Vec<i32> = cards.chars()
        .map(|c| get_card_value(c, joker_rules))
        .collect();
    
    for &v in &card_values {
        *counts.entry(v).or_insert(0) += 1;
    }
    
    let num_jokers = if joker_rules {
        counts.remove(&1).unwrap_or(0)
    } else {
        0
    };
    
    let mut frequencies: Vec<i32> = counts.values().cloned().collect();
    frequencies.sort_unstable();
    
    if let Some(last) = frequencies.last_mut() {
        *last += num_jokers;
    } else if num_jokers > 0 {
        frequencies.push(num_jokers);
    }
    
    let hand_rank = match frequencies.as_slice() {
        [5] => 7,             // five of a kind
        [1, 4] => 6,          // four of a kind
        [2, 3] => 5,          // full house
        [1, 1, 3] => 4,       // three of a kind
        [1, 2, 2] => 3,       // two pair
        [1, 1, 1, 2] => 2,    // one pair
        [1, 1, 1, 1, 1] => 1, // high card
        _ => panic!("Invalid hand: {}", cards),
    };
    
    (hand_rank, card_values)
}

fn parse_input(content: &str) -> Vec<Hand> {
    content.lines()
        .map(|line| {
            let parts: Vec<&str> = line.split_whitespace().collect();
            Hand {
                cards: parts[0].to_string(),
                bid: parts[1].parse().unwrap(),
            }
        })
        .collect()
}

fn calculate_winnings(mut hands: Vec<Hand>, joker_rules: bool) -> i64 {
    hands.sort_by_key(|hand| {
        let (type_rank, card_values) = get_hand_type(&hand.cards, joker_rules);
        (type_rank, card_values)
    });
    
    hands.iter()
        .enumerate()
        .map(|(i, hand)| (i as i64 + 1) * hand.bid)
        .sum()
}

fn main() {
    let content = read_to_string("input/input_07.txt").unwrap();
    let hands = parse_input(&content);
    
    println!("{}", calculate_winnings(hands.clone(), false));
    
    println!("{}", calculate_winnings(hands, true));
}