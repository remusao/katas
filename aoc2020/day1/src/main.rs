use std::time::Instant;

fn solve1(numbers: Vec<usize>) -> Option<usize> {
    let mut sorted: [bool; 2021] = [false; 2021];
    for n in numbers.into_iter() {
        sorted[n] = true;
    }

    for i in 0..=2020 {
        let j = 2020 - i;
        if sorted[i] && sorted[j] {
            return Some(i * j);
        }
    }

    None
}

fn solve2(numbers: Vec<usize>) -> Option<usize> {
    let mut sorted: [bool; 2021] = [false; 2021];
    for n in numbers.into_iter() {
        sorted[n] = true;
    }

    let mut non_zero: Vec<usize> = Vec::with_capacity(200);
    for (i, &c) in sorted.iter().enumerate() {
        if c {
            non_zero.push(i);
        }
    }

    for (index_i, i) in non_zero.iter().enumerate() {
        for j in non_zero
            .iter()
            .skip(index_i + 1)
            .take_while(|&j| j + i < 2020)
        {
            let k = 2020 - i - j;
            if sorted[k] {
                return Some(i * j * k);
            }
        }
    }

    None
}

fn main() {
    // let raw = include_str!("./example.txt");
    let raw = include_str!("./input1.txt");
    let numbers: Vec<usize> = raw
        .split('\n')
        .filter_map(|line| {
            let number = line.parse::<usize>().ok()?;
            if number > 2020 {
                None
            } else {
                Some(number)
            }
        })
        .collect();

    println!("{:?}", solve2(numbers.clone()));
    let start = Instant::now();
    // solve1(numbers);
    solve2(numbers);
    println!("Elapsed: {}", start.elapsed().as_micros());
}
