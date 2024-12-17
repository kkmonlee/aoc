use std::collections::HashMap;
use std::fs;

type Grid = Vec<Vec<char>>;

fn rotate(grid: &Grid) -> Grid {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut new_grid = vec![vec!['?'; rows]; cols];

    for r in 0..rows {
        for c in 0..cols {
            new_grid[c][rows - 1 - r] = grid[r][c];
        }
    }
    new_grid
}

fn roll(mut grid: Grid) -> Grid {
    let rows = grid.len();
    let cols = grid[0].len();

    for _ in 0..rows {
        for r in 1..rows {
            for c in 0..cols {
                if grid[r][c] == 'O' && grid[r - 1][c] == '.' {
                    grid[r][c] = '.';
                    grid[r - 1][c] = 'O';
                }
            }
        }
    }

    grid
}

fn score(grid: &Grid) -> usize {
    let rows = grid.len();
    let mut total = 0;

    for r in 0..rows {
        for c in 0..grid[0].len() {
            if grid[r][c] == 'O' {
                total += rows - r;
            }
        }
    }
    total
}

fn grid_to_tuple(grid: &Grid) -> Vec<Vec<char>> {
    grid.clone()
}

fn main() {
    let input = fs::read_to_string("input/input_14.txt").expect("Unable to read input file");
    let mut grid: Grid = input.lines().map(|line| line.chars().collect()).collect();

    let mut by_grid: HashMap<Grid, usize> = HashMap::new();
    let target = 1_000_000_000;
    let mut t = 0;

    while t < target {
        t += 1;
        for j in 0..4 {
            grid = roll(grid);
            if t == 1 && j == 0 {
                println!("Part 1: {}", score(&grid));
            }
            grid = rotate(&grid);
        }

        let grid_tuple = grid_to_tuple(&grid);

        if let Some(&prev_t) = by_grid.get(&grid_tuple) {
            let cycle_length = t - prev_t;
            let remaining_cycles = (target - t) / cycle_length;
            t += remaining_cycles * cycle_length;
        }
        by_grid.insert(grid_tuple, t);
    }

    println!("Part 2: {}", score(&grid));
}
