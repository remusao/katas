// Enter your code here
use std::io;
use std::io::prelude::*;
use std::vec::Vec;


fn main() {
    // Read stdin into a buffer
    let mut buffer = String::new();
    let mut stdin = io::stdin();
    let _ = stdin.read_to_string(&mut buffer);

    // Parse expected values
    let mut tokens = buffer
        .split_whitespace()
        .map(|token| token.parse().unwrap());

    let n: usize = tokens.next().unwrap();
    let d: usize = tokens.next().unwrap();
    let arr: Vec<usize> = tokens.collect();

    // Solve problem by doing d rotations on array
    for i in arr.iter().skip(d) {
        print!("{} ", i);
    }

    for i in arr.iter().take(d) {
        print!("{} ", i);
    }
}
