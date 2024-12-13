use std::fs::read_to_string;
use std::collections::HashMap;

fn parse_input(content: &str) -> (String, HashMap<String, (String, String)>) {
    let mut lines = content.lines();
    let directions = lines.next().unwrap().to_string();
    lines.next();
    
    let mut nodes = HashMap::new();
    for line in lines {
        let node = &line[0..3];
        let left = &line[7..10];
        let right = &line[12..15];
        nodes.insert(
            node.to_string(),
            (left.to_string(), right.to_string())
        );
    }
    
    (directions, nodes)
}

fn get_next_node(current: &str, direction: char, nodes: &HashMap<String, (String, String)>) -> String {
    let (left, right) = nodes.get(current).unwrap();
    match direction {
        'L' => left.clone(),
        'R' => right.clone(),
        _ => panic!("Invalid direction"),
    }
}

fn find_cycle(start: &str, directions: &str, nodes: &HashMap<String, (String, String)>) -> usize {
    let mut current = start.to_string();
    let mut steps = 0;
    
    for direction in directions.chars().cycle() {
        if current.ends_with('Z') {
            return steps;
        }
        current = get_next_node(&current, direction, nodes);
        steps += 1;
    }
    unreachable!()
}

fn lcm(nums: &[usize]) -> usize {
    if nums.len() == 1 {
        return nums[0];
    }
    let a = nums[0];
    let b = lcm(&nums[1..]);
    a * b / gcd(a, b)
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn main() {
    let content = read_to_string("input/input_08.txt").unwrap();
    let (directions, nodes) = parse_input(&content);
    
    let mut current = String::from("AAA");
    let mut steps = 0;
    for direction in directions.chars().cycle() {
        if current == "ZZZ" {
            break;
        }
        current = get_next_node(&current, direction, &nodes);
        steps += 1;
    }
    println!("{}", steps);
    
    let starting_nodes: Vec<String> = nodes.keys()
        .filter(|k| k.ends_with('A'))
        .cloned()
        .collect();
    
    let cycles: Vec<usize> = starting_nodes.iter()
        .map(|node| find_cycle(node, &directions, &nodes))
        .collect();
    
    println!("{}", lcm(&cycles));
}