use std::time::Instant;

use itertools::Itertools;

#[inline(always)]
fn solve(numbers: impl Iterator<Item = usize>) -> usize {
    numbers
        .tuple_windows()
        .filter(|(prev, next)| next > prev)
        .count()
}

fn main() {
    let numbers: Vec<usize> = include_str!("../input.txt")
        .split('\n')
        .filter_map(|line| line.parse::<usize>().ok())
        .collect();

    println!("Part1: {:?}", solve(numbers.iter().copied()));
    println!(
        "Part2: {:?}",
        solve(numbers.iter().tuple_windows().map(|(a, b, c)| a + b + c))
    );

    let start = Instant::now();
    solve(numbers.iter().copied());
    println!("Part1 took: {} nanoseconds", start.elapsed().as_nanos());

    let start = Instant::now();
    solve(numbers.iter().tuple_windows().map(|(a, b, c)| a + b + c));
    println!("Part2 took: {} nanoseconds", start.elapsed().as_nanos());
}
