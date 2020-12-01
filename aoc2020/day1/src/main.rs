use std::time::Instant;

fn solve1(numbers: &[usize; 2021]) {
    for i in 0..=2020 {
        if numbers[i] != 0 {
            for j in i..=2020 {
                if (i + j == 2020) && numbers[j] != 0 {
                    println!("? {} {} {}", i, j, i * j);
                }
            }
        }
    }
}

fn solve2(numbers: &[usize; 2021]) {
    for i in 0..=2020 {
        if numbers[i] != 0 {
            for j in i..=2020 {
                if (i + j) < 2020 && numbers[j] != 0 {
                    for k in j..=2020 {
                        if (i + j + k == 2020) && numbers[k] != 0 {
                            println!("? {} {} {} {}", i, j, k, k * i * j);
                        }
                    }
                }
            }
        }
    }
}

fn solve2_2(numbers: &[usize; 2021]) {
    let non_zero: Vec<usize> = numbers
        .iter()
        .enumerate()
        .filter_map(|(i, v)| if *v == 0 { None } else { Some(i) })
        .collect();

    for (index_i, i) in non_zero.iter().enumerate() {
        for (index_j, j) in non_zero.iter().enumerate().skip(index_i) {
            if i + j >= 2020 {
                break;
            }

            for k in non_zero.iter().skip(index_j) {
                let sum = i + j + k;
                if sum > 2020 {
                    break;
                }

                if sum == 2020 {
                    println!("? {} {} {} {}", i, j, k, k * i * j);
                    return;
                }
            }
        }
    }
}

fn main() {
    // let raw = include_str!("./example.txt");
    let raw = include_str!("./input1.txt");
    let mut numbers: [usize; 2021] = [0; 2021];
    for number in raw.split('\n').filter_map(|line| {
        let number = line.parse::<usize>().ok()?;
        if number > 2020 {
            None
        } else {
            Some(number)
        }
    }) {
        numbers[number] += 1;
    }

    // solve1(&numbers);

    let start = Instant::now();
    solve2_2(&numbers);
    println!("Elapsed: {}", start.elapsed().as_micros());
}
