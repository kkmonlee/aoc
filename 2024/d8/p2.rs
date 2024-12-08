use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::{HashMap, HashSet};

fn main() -> std::io::Result<()> {
  let file = File::open("input.txt")?;
  let ls: Vec<String> = BufReader::new(file).lines().map(|l| l.unwrap().trim().to_string()).collect();
  let rows = ls.len();
  let cols = if rows > 0 { ls[0].len() } else {0};

  // freq map
  let mut antennas_by_freq: HashMap<char, Vec<(i32,i32)>> = HashMap::new();
  for (y, line) in ls.iter().enumerate() {
    for (x, c) in line.chars().enumerate() {
      if c != '.' {
        antennas_by_freq.entry(c).or_default().push((x as i32, y as i32));
      }
    }
  }

  let mut antinodes: HashSet<(i32,i32)> = HashSet::new();

  for coords in antennas_by_freq.values() {
    if coords.len() < 2 { continue; }
    process_frequency(coords, &ls, &mut antinodes);
  }

  println!("{}", antinodes.len());
  Ok(())
}

fn my_gcd(a: i32, b: i32) -> i32 {
  if b == 0 { a.abs() } else { my_gcd(b, a % b) }
}

fn normalize_dir(dx: i32, dy: i32) -> (i32, i32) {
  let g = my_gcd(dx, dy);
  let (dx, dy) = (dx/g, dy/g);
  if dx < 0 || (dx == 0 && dy < 0) { (-dx, -dy) } else { (dx, dy) }
}

fn line_signature(x1: i32, y1: i32, dx: i32, dy: i32) -> (i32,i32,i32) {
  let c = dy*x1 - dx*y1;
  let g = my_gcd(my_gcd(dx.abs(), dy.abs()), c.abs());
  (dx/g, dy/g, c/g)
}

fn add_line_points(sx: i32, sy: i32, dx: i32, dy: i32, cols: i32, rows: i32, set: &mut HashSet<(i32,i32)>) {
  // fwd
  {
    let (mut x, mut y) = (sx, sy);
    while x>=0 && x<cols && y>=0 && y<rows {
      set.insert((x,y));
      x += dx; y += dy;
    }
  }
  // bwd
  {
    let (mut x, mut y) = (sx, sy);
    while x>=0 && x<cols && y>=0 && y<rows {
      set.insert((x,y));
      x -= dx; y -= dy;
    }
  }
}

fn process_frequency(coords: &[(i32,i32)], ls: &[String], antinodes: &mut HashSet<(i32,i32)>) {
  let mut visited_lines: HashSet<(i32,i32,i32)> = HashSet::new();
  let n = coords.len();
  let rows = ls.len() as i32;
  let cols = if rows > 0 { ls[0].len() as i32 } else {0};
  
  for i in 0..n {
    for j in i+1..n {
      let (x1,y1) = coords[i];
      let (x2,y2) = coords[j];
      let (dx,dy) = normalize_dir(x2 - x1, y2 - y1);
      let sig = line_signature(x1,y1,dx,dy);
      if !visited_lines.contains(&sig) {
        add_line_points(x1,y1,dx,dy,cols,rows,antinodes);
        visited_lines.insert(sig);
      }
    }
  }
}
