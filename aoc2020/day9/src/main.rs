fn find(n: usize, s: &[usize]) -> Option<(usize, usize)> {
    for i in s.iter() {
        for j in s.iter() {
            if j != i && i + j == n {
                return Some((*i, *j));
            }
        }
    }

    None
}

fn solve1(preamble_size: usize, numbers: &[usize]) -> Option<usize> {
    for i in preamble_size..numbers.len() {
        if find(numbers[i], &numbers[i - preamble_size..i]).is_none() {
            return Some(numbers[i]);
        }
    }

    None
}

fn main() {
    let raw = include_str!("./input1.txt");
    let numbers: Vec<usize> = raw
        .split('\n')
        .filter_map(|line| line.parse::<usize>().ok())
        .collect();

    if let Some(n) = solve1(25, &numbers) {
        println!("Part 1: {}", n);

        let mut lo = 0;
        let mut hi = 1;
        let mut s = numbers[lo] + numbers[hi];

        while s != n && hi < numbers.len() {
            while s < n && hi < numbers.len() {
                hi += 1;
                s += numbers[hi];
            }

            while s > n && lo < hi {
                s -= numbers[lo];
                lo += 1;
            }
        }

        let min: usize = *numbers[lo..=hi].iter().min().unwrap();
        let max: usize = *numbers[lo..=hi].iter().max().unwrap();
        println!("Part 2: {}", min + max);
    }
}
