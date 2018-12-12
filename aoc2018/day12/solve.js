const N = 70;

function count(state) {
  const initialSize = state.length / (2 * N + 1);
  const offset = initialSize * N;

  let total = 0;
  for (let i = 0; i < state.length; i += 1) {
    if (state[i] === 1) {
      total += i - offset;
    }
  }

  return total;
}

function pprint(state) {
  console.log(count(state));
  return;
  console.log(
    `${count(state)}\t${[...state].map(s => (s === 1 ? "#" : ".")).join("")}`
  );
}

function readInput(input) {
  const lines = input.trim().split("\n");

  // Initial state
  const initial = lines[0];
  const state = new Uint8Array(initial.length * (2 * N + 1));
  for (let i = 0; i < initial.length; i += 1) {
    state[i + N * initial.length] = initial[i] === "#" ? 1 : 0;
  }

  // Transitions
  const transitions = new Uint8Array(32);
  for (let i = 1; i < lines.length; i += 1) {
    const transition = lines[i];
    if (transition[9] === "#") {
      transitions[
        (transition[0] === "#" ? 1 : 0) |
          (transition[1] === "#" ? 2 : 0) |
          (transition[2] === "#" ? 4 : 0) |
          (transition[3] === "#" ? 8 : 0) |
          (transition[4] === "#" ? 16 : 0)
      ] = 1;
    }
  }

  return { state, transitions };
}

function main() {
  const example = `
#..#.#..##......###...###
...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #
`;
  const input = `
#.#####.##.###...#...#.####..#..#.#....##.###.##...#####.#..##.#..##..#..#.#.#.#....#.####....#..#
##### => #
####. => #
###.. => #
##.## => #
##.#. => #
##... => #
#.### => #
#..## => #
#...# => #
.#### => #
.###. => #
.##.# => #
.##.. => #
.#.#. => #
..#.# => #
..#.. => #
...#. => #
###.# => .
##..# => .
#.##. => .
#.#.# => .
#.#.. => .
#..#. => .
#.... => .
.#.## => .
.#..# => .
.#... => .
..### => .
..##. => .
...## => .
....# => .
..... => .
`;
  const { state, transitions } = readInput(input);

  // offset is the index of the 0 (starting point)
  let state1 = state;
  let state2 = state1.slice();
  let lastCount = 0;
  let diff = 1;

  for (let i = 0; i < 200; i += 1) {
    const newCount = count(state1);
    if (i === 20) {
      console.log("Part 1:", newCount);
    }
    diff = Math.abs(newCount - lastCount);
    lastCount = newCount;

    for (let j = 0; j < state1.length; j += 1) {
      state2[j + 2] =
        transitions[
          (state1[j] === 1 ? 1 : 0) |
            (state1[j + 1] === 1 ? 2 : 0) |
            (state1[j + 2] === 1 ? 4 : 0) |
            (state1[j + 3] === 1 ? 8 : 0) |
            (state1[j + 4] === 1 ? 16 : 0)
        ];
    }

    // Swap arrays
    let s = state1;
    state1 = state2;
    state2 = s;
  }

  console.log(
    "Part 2:",
    BigInt(count(state1)) + (50000000000n - 200n) * BigInt(diff)
  );
}

main();
