use std::fs::read_to_string;

#[derive(Debug)]
struct RangeMap {
    dest_start: i64,
    source_start: i64,
    length: i64,
}

fn parse_input(content: &str) -> (Vec<i64>, Vec<Vec<RangeMap>>) {
    let mut sections = content.split("\n\n");
    
    let seeds_line = sections.next().unwrap();
    let seeds: Vec<i64> = seeds_line
        .split(": ")
        .nth(1)
        .unwrap()
        .split_whitespace()
        .map(|n| n.parse().unwrap())
        .collect();
    
    let maps: Vec<Vec<RangeMap>> = sections
        .map(|section| {
            let lines: Vec<&str> = section.lines().collect();
            lines[1..].iter()
                .map(|line| {
                    let nums: Vec<i64> = line
                        .split_whitespace()
                        .map(|n| n.parse().unwrap())
                        .collect();
                    RangeMap {
                        dest_start: nums[0],
                        source_start: nums[1],
                        length: nums[2],
                    }
                })
                .collect()
        })
        .collect();
    
    (seeds, maps)
}

fn do_map(map: &[RangeMap], value: i64) -> i64 {
    for range in map {
        if range.source_start <= value && value < range.source_start + range.length {
            return value - range.source_start + range.dest_start;
        }
    }
    value
}

fn map_range(map: &[RangeMap], start: i64, end: i64) -> Vec<(i64, i64)> {
    let mut result = Vec::new();
    let mut processed = false;
    
    for range in map {
        if range.source_start < end && start < range.source_start + range.length {
            let range_start = start.max(range.source_start);
            let range_end = end.min(range.source_start + range.length);
            
            result.push((
                range_start - range.source_start + range.dest_start,
                range_end - range.source_start + range.dest_start
            ));
            
            if start < range.source_start {
                result.extend(map_range(map, start, range.source_start));
            }
            if end > range.source_start + range.length {
                result.extend(map_range(map, range.source_start + range.length, end));
            }
            
            processed = true;
            break;
        }
    }
    
    if !processed {
        result.push((start, end));
    }
    
    result
}

fn flatten_ranges(mut ranges: Vec<(i64, i64)>) -> Vec<(i64, i64)> {
    if ranges.is_empty() {
        return ranges;
    }
    
    ranges.sort_unstable_by_key(|&(start, _)| start);
    let mut result = vec![ranges[0]];
    
    for &(start, end) in &ranges[1..] {
        let last = result.last_mut().unwrap();
        if start <= last.1 {
            last.1 = last.1.max(end);
        } else {
            result.push((start, end));
        }
    }
    
    result
}

fn map_ranges(map: &[RangeMap], ranges: Vec<(i64, i64)>) -> Vec<(i64, i64)> {
    let mut new_ranges = Vec::new();
    for (start, end) in ranges {
        new_ranges.extend(map_range(map, start, end));
    }
    flatten_ranges(new_ranges)
}

fn main() {
    let content = read_to_string("input/input_05.txt").unwrap();
    let (seeds, maps) = parse_input(&content);
    
    let mut locations = seeds.clone();
    for map in &maps {
        locations = locations.iter()
            .map(|&value| do_map(map, value))
            .collect();
    }
    println!("{}", locations.iter().min().unwrap());
    
    let mut ranges: Vec<(i64, i64)> = seeds.chunks(2)
        .map(|chunk| (chunk[0], chunk[0] + chunk[1]))
        .collect();
    
    for map in &maps {
        ranges = map_ranges(map, ranges);
    }
    
    println!("{}", ranges[0].0);
}