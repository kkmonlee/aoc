use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::{HashMap, HashSet};
use std::char;
use std::iter::FromIterator;

fn main() -> std::io::Result<()> {
  let file = File::open("input/input_8.txt")?;
  let grid: Vec<String> = BufReader::new(file).lines().map(|l| l.unwrap()).collect();
  let rows = grid.len();
  let cols = if rows > 0 { grid[0].len() } else {0};

  let antennas_by_freq = parse_map(&grid);

  let antinodes_part1 = computeAntinodesPart1(&antennas_by_freq);
  let valid_part1 = antinodes_part1.into_iter()
                                   .filter(|(r,c)| *r >=0 && *r<rows as i32 && *c>=0 && *c<cols as i32)
                                   .count();
                                   
  let antinodes_part2 = computeAntinodesPart2(&grid, &antennas_by_freq);
  let valid_part2 = antinodes_part2.len();

  println!("Part 1: {}", valid_part1);
  println!("Part 2: {}", valid_part2);

  Ok(())
}

fn parse_map(grid: &[String]) -> HashMap<char, Vec<(i32,i32)>> {
  let mut m = HashMap::new();
  for (r, line) in grid.iter().enumerate() {
    for (c, ch) in line.chars().enumerate() {
      if ch.is_ascii_alphanumeric() {
        m.entry(ch).or_default().push((r as i32, c as i32));
      }
    }
  }
  m
}

fn computeAntinodesPart1(antennas_by_freq: &HashMap<char, Vec<(i32,i32)>>) -> HashSet<(i32,i32)> {
  let mut all = HashSet::new();
  for positions in antennas_by_freq.values() {
    let n = positions.len();
    for i in 0..n {
      for j in i+1..n {
        let (r1,c1) = positions[i];
        let (r2,c2) = positions[j];
        let dist = (r2 - r1, c2 - c1);
        if dist == (0,0) { continue; }
        let antinode1 = (r1 - dist.0, c1 - dist.1);
        let antinode2 = (r2 + dist.0, c2 + dist.1);
        all.insert(antinode1);
        all.insert(antinode2);
      }
    }
  }
  all
}

fn computeAntinodesPart2(grid: &[String], antennas_by_freq: &HashMap<char, Vec<(i32,i32)>>) -> HashSet<(i32,i32)> {
  let mut result = HashSet::new();
  for coords in antennas_by_freq.values() {
    if coords.len() < 2 { continue; }
    process_frequency_part2(coords, grid, &mut result);
  }
  result
}

fn my_gcd(a: i32, b: i32) -> i32 {
  if b == 0 { a.abs() } else { my_gcd(b, a % b) }
}

fn normalize_dir(dx: i32, dy: i32) -> (i32,i32) {
  let g = my_gcd(dx,dy);
  let (dx,dy) = (dx/g, dy/g);
  if dx<0 || (dx==0 && dy<0) {(-dx,-dy)} else {(dx,dy)}
}

fn line_signature(x1:i32, y1:i32, dx:i32, dy:i32) -> (i32,i32,i32) {
  let c = dy*x1 - dx*y1;
  let g = my_gcd(my_gcd(dx.abs(),dy.abs()), c.abs());
  (dx/g, dy/g, c/g)
}

fn add_line_points(sx:i32, sy:i32, dx:i32, dy:i32, cols:i32, rows:i32, set:&mut HashSet<(i32,i32)>) {
  // fwd
  {
    let (mut x,mut y)=(sx,sy);
    while x>=0 && y>=0 && x<rows && y<cols {
      set.insert((x,y));
      x+=dx; y+=dy;
    }
  }
  // bwd
  {
    let (mut x,mut y)=(sx,sy);
    while x>=0 && y>=0 && x<rows && y<cols {
      set.insert((x,y));
      x-=dx; y-=dy;
    }
  }
}

fn process_frequency_part2(coords: &[(i32,i32)], grid: &[String], set:&mut HashSet<(i32,i32)>) {
  let mut visited = HashSet::new();
  let n = coords.len() as usize;
  let rows = grid.len() as i32;
  let cols = if rows>0 {grid[0].len() as i32} else {0};
  for i in 0..n {
    for j in i+1..n {
      let (x1,y1)=coords[i];
      let (x2,y2)=coords[j];
      let (dx,dy)=normalize_dir(x2-x1,y2-y1);
      let sig = line_signature(x1,y1,dx,dy);
      if !visited.contains(&sig) {
        add_line_points(x1,y1,dx,dy,cols,rows,set);
        visited.insert(sig);
      }
    }
  }
}
