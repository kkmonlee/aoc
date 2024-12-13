use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug)]
struct Game {
    rounds: Vec<Vec<(i32, String)>>
}

fn parse_line(line: &str) -> Game {
    let parts: Vec<&str> = line.split(':').collect();
    let rounds_str = parts[1].trim();

    let rounds: Vec<Vec<(i32, String)>> = rounds_str
        .split(';')
        .map(|round| {
            round.trim()
                .split(',')
                .map(|cube| {
                    let parts: Vec<&str> = cube.trim().split_whitespace().collect();
                    let count = parts[0].parse::<i32>().unwrap();
                    let color = parts[1].to_string();
                    (count, color)
                })
                .collect()
        })
        .collect();

    Game { rounds }
}

fn is_possible(game: &Game, caps: &HashMap<String, i32>) -> bool {
    game.rounds.iter().all(|round| {
        round.iter().all(|(count, color)| {
            count <= caps.get(color).unwrap_or(&0)
        })
    })
}

fn power(game: &Game) -> i32 {
    let mut mins = HashMap::new();

    for round in &game.rounds {
        for (count, color) in round {
            let entry = mins.entry(color.clone()).or_insert(0);
            *entry = (*entry).max(*count);
        }
    }

    ["red", "green", "blue"]
        .iter()
        .map(|&color| mins.get(&color.to_string()).unwrap_or(&0))
        .product()
}

fn main() -> io::Result<()> {
    let mut caps = HashMap::new();
    caps.insert("red".to_string(), 12);
    caps.insert("green".to_string(), 13);
    caps.insert("blue".to_string(), 14);

    let file = File::open("input/input_02.txt")?;
    let reader = io::BufReader::new(file);
    let games: Vec<Game> = reader
        .lines()
        .filter_map(|line| line.ok())
        .map(|line| parse_line(&line))
        .collect();

    let part1: usize = games.iter()
        .enumerate()
        .filter(|(_, game)| is_possible(game, &caps))
        .map(|(i, _)| i + 1)
        .sum();

    let part2: i32 = games.iter()
        .map(power)
        .sum();

    println!("{}", part1);
    println!("{}", part2);

    Ok(())
}