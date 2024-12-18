use std::collections::{HashSet, VecDeque};
use std::fs::read_to_string;

type Point = (i32, i32);
type Grid = Vec<Vec<bool>>;

struct PathFinder {
    grid: Grid,
    size: i32,
    target: Point,
}

impl PathFinder {
    fn new(size: i32) -> Self {
        Self {
            grid: vec![vec![false; size as usize]; size as usize],
            size,
            target: (size - 1, size - 1),
        }
    }

    fn clear(&mut self) {
        self.grid.iter_mut().for_each(|row| row.fill(false));
    }

    fn add_byte(&mut self, (x, y): Point) {
        self.grid[y as usize][x as usize] = true;
    }

    fn in_bounds(&self, x: i32, y: i32) -> bool {
        (0..self.size).contains(&x) && (0..self.size).contains(&y)
    }

    fn neighbors(&self, (x, y): Point) -> impl Iterator<Item = Point> + '_ {
        const DIRS: [(i32, i32); 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];
        DIRS.iter()
            .map(move |(dx, dy)| (x + dx, y + dy))
            .filter(move |&(nx, ny)| self.in_bounds(nx, ny) && !self.grid[ny as usize][nx as usize])
    }

    fn can_reach_target(&self) -> bool {
        if self.grid[0][0] || self.grid[self.target.1 as usize][self.target.0 as usize] {
            return false;
        }

        let mut visited = HashSet::from([(0, 0)]);
        let mut queue = VecDeque::from([(0, 0)]);

        while let Some(pos) = queue.pop_front() {
            if pos == self.target {
                return true;
            }

            self.neighbors(pos)
                .filter(|p| visited.insert(*p))
                .for_each(|p| queue.push_back(p));
        }
        false
    }

    fn find_shortest_path(&self) -> Option<i32> {
        if self.grid[0][0] || self.grid[self.target.1 as usize][self.target.0 as usize] {
            return None;
        }

        let mut start_visited = HashSet::from([(0, 0)]);
        let mut end_visited = HashSet::from([self.target]);
        let mut start_queue = VecDeque::from([((0, 0), 0)]);
        let mut end_queue = VecDeque::from([(self.target, 0)]);

        while !start_queue.is_empty() && !end_queue.is_empty() {
            // expand fwd
            if let Some((pos, dist)) = start_queue.pop_front() {
                if end_visited.contains(&pos) {
                    return Some(
                        dist + end_queue
                            .iter()
                            .find(|&&(p, _)| p == pos)
                            .map(|&(_, d)| d)
                            .unwrap(),
                    );
                }

                self.neighbors(pos)
                    .filter(|p| start_visited.insert(*p))
                    .for_each(|p| start_queue.push_back((p, dist + 1)));
            }

            // expand bwd
            if let Some((pos, dist)) = end_queue.pop_front() {
                if start_visited.contains(&pos) {
                    return Some(
                        dist + start_queue
                            .iter()
                            .find(|&&(p, _)| p == pos)
                            .map(|&(_, d)| d)
                            .unwrap(),
                    );
                }

                self.neighbors(pos)
                    .filter(|p| end_visited.insert(*p))
                    .for_each(|p| end_queue.push_back((p, dist + 1)));
            }
        }
        None
    }
}

fn parse_input(content: &str) -> Vec<Point> {
    content
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let mut parts = line.split(',');
            let x = parts.next().unwrap().parse().unwrap();
            let y = parts.next().unwrap().parse().unwrap();
            (x, y)
        })
        .collect()
}

fn solve_both_parts(coordinates: &[Point]) -> (Option<i32>, Point) {
    let mut pathfinder = PathFinder::new(71);

    // shortest path after 1024 bytes
    coordinates
        .iter()
        .take(1024)
        .for_each(|&p| pathfinder.add_byte(p));
    let part1 = pathfinder.find_shortest_path();

    // search for blocking byte
    let mut left = 0;
    let mut right = coordinates.len() - 1;
    let mut last_reachable = 0;

    while left <= right {
        let mid = (left + right) / 2;
        pathfinder.clear();

        coordinates
            .iter()
            .take(mid + 1)
            .for_each(|&p| pathfinder.add_byte(p));

        if pathfinder.can_reach_target() {
            last_reachable = mid;
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }

    (part1, coordinates[last_reachable + 1])
}

fn main() -> std::io::Result<()> {
    let content = read_to_string("input/input_18.txt")?;
    let coordinates = parse_input(&content);

    let (part1, (block_x, block_y)) = solve_both_parts(&coordinates);

    println!("Part 1: {}", part1.unwrap());
    println!("Part 2: {},{}", block_x, block_y);

    Ok(())
}
