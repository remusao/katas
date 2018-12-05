const fs = require("fs");

function readInput() {
  const input = fs
    .readFileSync(process.argv[process.argv.length - 1], { encoding: "utf-8" })
    .trim();
  const buffer = new Uint8Array(input.length);
  for (let i = 0; i < input.length; i += 1) {
    buffer[i] = input.charCodeAt(i);
  }
  return buffer;
}

function reduce(input) {
  const buffer = new Uint8Array(input.length);
  let index = 0;

  for (let i = 0; i < input.length; i += 1) {
    buffer[index++] = input[i];

    // Fold left
    while (index > 1 && (buffer[index - 1] ^ buffer[index - 2]) === 32) {
      index -= 2;
    }
  }

  return index;
}

function part1(input) {
  return reduce(input);
}

function part2(input) {
  const a = 97; // 'a'
  let minimumSize = input.length;
  for (let i = 0; i < 26; i += 1) {
    const forbiddenLetter = a + i;
    minimumSize = Math.min(
      minimumSize,
      reduce(input.filter(ch => (ch | 32) !== forbiddenLetter))
    );
  }
  return minimumSize;
}

function main() {
  const input = readInput();

  console.log("Part 1:", part1(input));
  console.log("Part 2:", part2(input));

  // const t0 = Date.now();
  // for (let i = 0; i < 100; i += 1) {
  //   part2(input);
  // }
  // const total = (Date.now() - t0) / 100;
  // console.log("Time", total);
}

main();
