use std::fs::read_to_string;

fn parse_sequence(line: &str) -> Vec<i64> {
    line.split_whitespace()
        .map(|n| n.parse().unwrap())
        .collect()
}

fn extrapolate_forward(sequence: &[i64]) -> i64 {
    let mut sequences = vec![sequence.to_vec()];
    
    while !sequences.last().unwrap().iter().all(|&x| x == 0) {
        let current = sequences.last().unwrap();
        let diffs: Vec<i64> = current.windows(2)
            .map(|w| w[1] - w[0])
            .collect();
        sequences.push(diffs);
    }
    
    sequences.iter()
        .map(|seq| seq.last().unwrap())
        .sum()
}

fn extrapolate_backward(sequence: &[i64]) -> i64 {
    let mut sequences = vec![sequence.to_vec()];
    
    while !sequences.last().unwrap().iter().all(|&x| x == 0) {
        let current = sequences.last().unwrap();
        let diffs: Vec<i64> = current.windows(2)
            .map(|w| w[1] - w[0])
            .collect();
        sequences.push(diffs);
    }
    
    sequences.iter()
        .rev()
        .fold(0, |acc, seq| seq[0] - acc)
}

fn main() {
    let content = read_to_string("input/input_09.txt").unwrap();
    let sequences: Vec<Vec<i64>> = content
        .lines()
        .map(parse_sequence)
        .collect();
    
    let sum_forward: i64 = sequences.iter()
        .map(|seq| extrapolate_forward(seq))
        .sum();
    println!("{}", sum_forward);
    
    let sum_backward: i64 = sequences.iter()
        .map(|seq| extrapolate_backward(seq))
        .sum();
    println!("{}", sum_backward);
}