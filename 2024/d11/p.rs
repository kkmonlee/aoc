use std::collections::HashMap;

fn simulate_blinks(stones: Vec<u64>, blinks: usize) -> u64 {
    let mut stone_counts: HashMap<u64, u64> = HashMap::new();

    for stone in stones {
        *stone_counts.entry(stone).or_insert(0) += 1;
    }

    for _ in 0..blinks {
        let mut next_counts: HashMap<u64, u64> = HashMap::new();

        for (&stone, &count) in stone_counts.iter() {
          if stone == 0 {
              *next_counts.entry(1).or_insert(0) += count;
          } else {
              let num_digits = ((stone as f64).log10().floor() as usize) + 1;
              if num_digits % 2 == 0 {
                  // Split stone with even number of digits
                  let half_len = num_digits / 2;
                  let split_point = 10u64.pow(half_len as u32);
                  let left = stone / split_point;
                  let right = stone % split_point;
                  *next_counts.entry(left).or_insert(0) += count;
                  *next_counts.entry(right).or_insert(0) += count;
              } else {
                  // Multiply by 2024 for stones with odd digits
                  *next_counts.entry(stone * 2024).or_insert(0) += count;
              }
          }
        }

        stone_counts = next_counts;
    }

    stone_counts.values().sum()
}

fn main() {
    let stones = vec![0, 7, 6618216, 26481, 885, 42, 202642, 8791];
    
    let result_25 = simulate_blinks(stones.clone(), 25);
    let result_75 = simulate_blinks(stones, 75);

    println!("Part 1: {}", result_25);
    println!("Part 2: {}", result_75);
}
