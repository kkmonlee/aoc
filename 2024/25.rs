use std::fs;

fn fits(key: &str, lock: &str) -> bool {
    let key_lines: Vec<&str> = key.lines().collect();
    let lock_lines: Vec<&str> = lock.lines().collect();
    
    let rows = key_lines.len();
    let cols = key_lines[0].len();
    
    assert_eq!(rows, lock_lines.len());
    assert_eq!(cols, lock_lines[0].len());
    
    for r in 0..rows {
        for c in 0..cols {
            if key_lines[r].chars().nth(c).unwrap() == '#' && 
               lock_lines[r].chars().nth(c).unwrap() == '#' {
                return false;
            }
        }
    }
    true
}

fn main() {
    let contents = fs::read_to_string("input/input_25.txt")
        .expect("should have been able to read the file");
    
    let shapes: Vec<&str> = contents.trim().split("\n\n").collect();
    let mut keys = Vec::new();
    let mut locks = Vec::new();
    
    for shape in shapes {
        let first_line = shape.lines().next().unwrap();
        if first_line.starts_with('#') {
            locks.push(shape);
        } else {
            keys.push(shape);
        }
    }
    
    let mut ans = 0;
    for key in &keys {
        for lock in &locks {
            if fits(key, lock) {
                ans += 1;
            }
        }
    }
    
    println!("{}", ans);
}