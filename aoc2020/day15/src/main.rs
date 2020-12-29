use std::iter;
use std::time::Instant;

fn run(numbers: &[usize], stop: usize) -> usize {
    let now = Instant::now();
    let mut last_seen: Vec<usize> = iter::repeat(0).take(stop).collect();
    for (i, n) in numbers.iter().take(numbers.len() - 1).enumerate() {
        last_seen[*n] = i + 1;
    }
    let elapsed = now.elapsed();
    println!("Elapsed: {}", elapsed.as_millis());

    let mut i = numbers.len();
    let mut n0 = numbers[i - 1];
    let mut l = last_seen[n0];
    last_seen[n0] = i;

    while i != stop {
        n0 = if l == 0 { 0 } else { i - l };
        i += 1;
        l = last_seen[n0];
        last_seen[n0] = i;
    }

    n0
}

fn main() {
    let numbers = vec![9, 12, 1, 4, 17, 0, 18];
    println!("{}", run(&numbers, 2020));

    let now = Instant::now();
    let n = run(&numbers, 30000000);
    let elapsed = now.elapsed();
    println!("Elapsed: {}", elapsed.as_millis());
    println!("{}", n);
}
